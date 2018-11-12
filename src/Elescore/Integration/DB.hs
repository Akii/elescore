{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Elescore.Integration.DB
  ( runDBSource
  ) where

import           ClassyPrelude                          hiding (for, forM, head)
import           Data.DateTime                          (toSqlString)
import           Data.Map                               (elems)
import           Network.HTTP.Client                    (Manager)
import           Pipes
import           Pipes.Concurrent
import qualified Pipes.Prelude                          as P
import           Prelude                                (head)

import           Database.SimpleEventStore
import           Elescore.IdTypes
import           Elescore.Integration.Common.Monitoring
import           Elescore.Integration.Common.Types
import           Elescore.Integration.Common.Utils
import           Elescore.Integration.DB.Client
import           Elescore.Integration.DB.Mapping
import           Elescore.Integration.DB.Monitoring
import           Elescore.Integration.DB.Types
import           Statistics.IQR

runDBSource :: MonadIO m => Store -> ApiKey -> Host -> Manager -> m (Source 'DB)
runDBSource store key host mgr = do
  disP <- disruptions store
  fP <- facilities store
  oEvs <- liftIO (readStream store)

  let oState = replayMkSeed dbObjectMonitor . fmap evPayload $ oEvs
      oIds = keys oState
      oP = objects store oEvs

  (output, input) <- liftIO (spawn unbounded)

  (out1, in1) <- liftIO (spawn unbounded)
  (out2, in2) <- liftIO (spawn unbounded)
  (out3, in3) <- liftIO (spawn unbounded)

  liftIO $ do
    void . forkIO . runEffect $ apiP 10 fetchDisruptedFacilities >-> disP >-> toOutput out1
    void . forkIO . runEffect $ apiP 3600 (fetchFacilities Nothing) >-> fP >-> toOutput (output <> out2)
    void . forkIO . runEffect $ fromInput input >-> P.map evPayload >-> stationFilterP oIds >-> fetchStationP 5 >-> oP >-> toOutput out3

  return Source
     { disruptionEvents = in1
     , facilityEvents = in2
     , objectEvents = in3
     }

  where
    stationFilterP :: MonadIO m => [ObjectId] -> Pipe (FacilityEvent 'DB) ObjectId m ()
    stationFilterP = go
      where
        go s = do
          ev <- await
          case ev of
            FacilityAssignedToObject _ oid -> unless (oid `elem` s) (yield oid >> go (oid : s))
            _                              -> return ()
          go s

    fetchStationP :: Int -> Pipe ObjectId Station IO ()
    fetchStationP delay = forever $ do
      oid <- fromObjectId <$> await
      a <- liftIO $ runReaderT (fetchStation oid) (key, mgr, host)
      either print yield a
      waitSeconds delay

    apiP :: Int -> API a -> Producer a IO ()
    apiP delay action = forever $ do
      a <- liftIO $ runReaderT action (key, mgr, host)
      either print yield a
      waitSeconds delay

disruptions :: MonadIO m => Store -> m (Pipe [Facility] (PersistedEvent (DisruptionEvent 'DB)) IO ())
disruptions store = do
  disEvs <- liftIO (readStream store)

  return $ P.map (fmap mkMDisruption)
    >-> monitorP dbDisruptionMonitor (replayMkSeed dbDisruptionMonitor . fmap evPayload $ disEvs)
    >-> eventStoreP store
    >-> eachBefore (chunkBy (toSqlString . evOccurredOn) disEvs)
    >-> disruptedApiFilter
    >-> P.concat
    >-> filterUnknown

facilities :: MonadIO m => Store -> m (Pipe [Facility] (PersistedEvent (FacilityEvent 'DB)) IO ())
facilities store = do
  fEvs <- liftIO (readStream store)

  return $ P.map (fmap mkMFacility)
    >-> monitorP dbFacilityMonitor (replayMkSeed dbFacilityMonitor . fmap evPayload $ fEvs)
    >-> eventStoreP store
    >-> P.concat
    >-> eachBefore fEvs

objects :: Store -> [PersistedEvent (ObjectEvent 'DB)] -> Pipe Station (PersistedEvent (ObjectEvent 'DB)) IO ()
objects store oEvs =
  P.map mkMObject
    >-> monitorSP dbObjectMonitor (replayMkSeed dbObjectMonitor . fmap evPayload $ oEvs)
    >-> eventStoreP store
    >-> P.concat
    >-> eachBefore oEvs

-- | Filters out FaSta API unknown facility states
filterUnknown :: Pipe (PersistedEvent (DisruptionEvent a)) (PersistedEvent (DisruptionEvent a)) IO ()
filterUnknown = for cat $ \ev ->
  case evPayload ev of
    FacilityDisrupted _ r       -> unless (unknownReason r) (yield ev)
    DisruptionReasonUpdated _ r -> unless (unknownReason r) (yield ev)
    _                           -> yield ev

  where
    unknownReason :: Reason -> Bool
    unknownReason MonitoringNotAvailable = True
    unknownReason MonitoringDisrupted    = True
    unknownReason _                      = False

type Disruptions = MonitorState FacilityId MDisruption

disruptedApiFilter :: MonadIO m => Pipe [PersistedEvent (DisruptionEvent 'DB)] [PersistedEvent (DisruptionEvent 'DB)] m ()
disruptedApiFilter = do
  firstEvents <- await
  let initialState = deriveState mempty firstEvents
      initialSample = singletonSample . fromIntegral . length $ initialState
  go initialState initialSample Nothing

  where
    go :: MonadIO m => Disruptions -> Sample -> Maybe (Disruptions, [PersistedEvent (DisruptionEvent 'DB)]) -> Pipe [PersistedEvent (DisruptionEvent 'DB)] [PersistedEvent (DisruptionEvent 'DB)] m ()
    go previousState sample Nothing = do
      nextEvents <- await
      let nextState = deriveState previousState nextEvents
          numberOfDisruptions = fromIntegral (length nextState)

      if isResidual numberOfDisruptions sample
        then do
        liftIO (putStrLn $ (pack . toSqlString . evOccurredOn $ head nextEvents) <> " Residual detected: " <> tshow (length previousState) <> " -> " <> tshow numberOfDisruptions)
        go previousState sample (Just (nextState, nextEvents))
        else yield nextEvents >> go nextState (addSample numberOfDisruptions sample) Nothing
    go lastKnownGoodState sample (Just (previousState, accumulatedEvents)) = do
      nextEvents <- await
      let nextState = deriveState previousState nextEvents
          numberOfDisruptions = fromIntegral (length nextState)
      if isResidual numberOfDisruptions sample
        then go lastKnownGoodState sample (Just (nextState, accumulatedEvents ++ nextEvents))
        else do
        putStrLn $ (pack . toSqlString . evOccurredOn $ head nextEvents) <> " Back to normal: " <> tshow (length lastKnownGoodState) <> " -> " <> tshow numberOfDisruptions
        yield (compensateApiDisruption lastKnownGoodState nextState (accumulatedEvents ++ nextEvents))
        go nextState (addSample numberOfDisruptions sample) Nothing

    addSample :: Double -> Sample -> Sample
    addSample a = restrictSampleSize 1000 . insertSample' a

    deriveState = foldl' (flip applyEvent)

    applyEvent :: PersistedEvent (DisruptionEvent 'DB) -> Disruptions -> Disruptions
    applyEvent = apply dbDisruptionMonitor . evPayload

    -- | from all happened events, take the latest one that was supposed to be happening
    compensateApiDisruption :: Disruptions -> Disruptions -> [PersistedEvent (DisruptionEvent 'DB)] -> [PersistedEvent (DisruptionEvent 'DB)]
    compensateApiDisruption d1 d2 evs =
      let (_, events) = calculateChanges dbDisruptionMonitor d1 (elems d2)
          reversedEvents = reverse evs
      in foldl' (\acc ev -> acc ++ maybe mempty pure (find ((==) ev . evPayload) reversedEvents)) [] events

chunkBy :: Eq b => (a -> b) -> [a] -> [[a]]
chunkBy _  [] = []
chunkBy f (x:xs) =
  let (_, as, res) = foldl' doChunk (f x, [x], []) xs
  in reverse (reverse as : res)
  where
    doChunk (b, as, ass) a =
      if f a == b
      then (b, a : as, ass)
      else (f a, [a], reverse as : ass)
