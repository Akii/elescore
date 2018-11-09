{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Elescore.Integration.DB
  ( runDBSource
  ) where

import           ClassyPrelude                          hiding (for, forM)
import           Network.HTTP.Client                    (Manager)
import           Pipes
import           Pipes.Concurrent
import qualified Pipes.Prelude                          as P

import           Database.SimpleEventStore
import           Elescore.IdTypes
import           Elescore.Integration.Common.Monitoring
import           Elescore.Integration.Common.Types
import           Elescore.Integration.Common.Utils
import           Elescore.Integration.DB.Client
import           Elescore.Integration.DB.Mapping
import           Elescore.Integration.DB.Monitoring
import           Elescore.Integration.DB.Types

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
    >-> P.concat
    >-> eachBefore disEvs
    >-> filterUnknownP

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

-- | Filters out FaSta API unknown facility states effectively
filterUnknownP :: Pipe (PersistedEvent (DisruptionEvent a)) (PersistedEvent (DisruptionEvent a)) IO ()
filterUnknownP = for cat $ \ev ->
  case evPayload ev of
    FacilityDisrupted _ r       -> unless (unknownReason r) (yield ev)
    DisruptionReasonUpdated _ r -> unless (unknownReason r) (yield ev)
    _                           -> yield ev

  where
    unknownReason :: Reason -> Bool
    unknownReason MonitoringNotAvailable = True
    unknownReason MonitoringDisrupted    = True
    unknownReason _                      = False
