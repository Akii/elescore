{-# LANGUAGE RecordWildCards #-}

module Elescore.Pipeline
  ( elepipe
  ) where

import           ClassyPrelude                 hiding ((<>))
import           Data.Map                      (elems)
import           Data.Monoid                   ((<>))
import           Pipes
import           Pipes.Concurrent              hiding (New)
import qualified Pipes.Prelude                 as P

import           Elescore.Disruptions.History  (History, applyEventToHistory)
import           Elescore.Disruptions.Types
import           Elescore.Notifications
import           Elescore.Remote.Client
import           Elescore.Remote.DisruptionLog
import           Elescore.Remote.Monitoring
import           Elescore.Remote.StationCache
import           Elescore.Remote.Types         hiding (disId, disStationId)
import qualified Elescore.Remote.Types         as RT
import           Elescore.Repository
import           Elescore.Types
import           Elescore.Users.Types

elepipe :: [DisruptionEvent] -> Elescore ()
elepipe devs = do
  disRef <- currDisruptionsRef
  historyRef <- disruptionHistory
  dis <- liftIO (readIORef disRef)
  h <- liftIO (readIORef historyRef)

  (out1,in1) <- liftIO (spawn unbounded)
  (out2,in2) <- liftIO (spawn unbounded)
  (out3,in3) <- liftIO (spawn unbounded)

  (out4, in4) <- liftIO (spawn unbounded)
  (out5, in5) <- liftIO (spawn unbounded)

  runEffect $ each devs >-> P.map (disruptionToFacility . toDisruption . devDisruption) >-> toOutput out5

  let disEventP    = disruptionProducer 10 >-> disruptionEventPipe dis >-> toOutput (out1 <> out2 <> out3)
      disEvLogP    = fromInput in1 >-> eventLogPipe >-> printPipe >-> notificationPipe >-> disruptionsStatePipe dis >-> ioRefConsumer disRef
      disHistoryP  = fromInput in3 >-> disruptionHistoryPipe h >-> ioRefConsumer historyRef
      disFacilityP = fromInput in2 >-> P.map (disruptionToFacility . toDisruption . devDisruption) >-> toOutput out5
      facilityP    = facilityProducer 3600  >-> toOutput out4
      stationC     = fromInput (in4 <> in5) >-> stationFetchingPipe 5 >-> stationCacheConsumer

  elepar [runEffect disEventP,
          runEffect disEvLogP,
          runEffect disHistoryP,
          runEffect disFacilityP,
          runEffect facilityP,
          runEffect stationC]

disruptionProducer :: Int -> Producer Disruptions Elescore ()
disruptionProducer delay = forever $ do
  diss <- lift fetchDisruptions
  either print (yield . toDisruptions) diss
  waitSeconds delay
  where
    toDisruptions = foldr (liftA2 insertMap RT.disId id) mempty

facilityProducer :: Int -> Producer Facility Elescore ()
facilityProducer delay = forever $ do
  fs <- lift fetchFacilities
  either print (mapM_ (yield . toFacility)) fs
  waitSeconds delay

disruptionEventPipe :: Disruptions -> Pipe Disruptions DisruptionEvent Elescore ()
disruptionEventPipe = go
  where
    go st = do
      st' <- await
      evs <- mapM (\(d,c) -> liftIO (mkDisruptionEvent d c)) (calculateChanges st st')
      mapM_ yield evs
      go st'

printPipe :: Pipe DisruptionEvent DisruptionEvent Elescore ()
printPipe = forever $ do
  dev <- await
  logDisruptionEvent dev
  yield dev

  where
    logDisruptionEvent DisruptionEvent {..} =
      putStrLn $ "[" ++ tshow devOccurredOn ++ "][" ++ tshow devChangeType ++ "] " ++ tshow (unDisruptionId $ RT.disId devDisruption)

eventLogPipe :: Pipe DisruptionEvent DisruptionEvent Elescore ()
eventLogPipe = forever $ do
  dev <- await
  lift (appendDisruptionEvent dev)
  yield dev

notificationPipe :: Pipe DisruptionEvent DisruptionEvent Elescore ()
notificationPipe = forever $ do
  dev <- await
  lift (notifyAboutEventIfNecessary dev)
  yield dev

  where
    notifyAboutEventIfNecessary :: DisruptionEvent -> Elescore ()
    notifyAboutEventIfNecessary (DisruptionEvent _ _ disData New) = do
      ss <- stations
      let dis = toDisruption disData
          ms = lookup (disStationId dis) ss
          mf = lookup (disFacilityId dis) . sFacilities =<< ms
      subscribedUsers <- userWatchingFacility (disFacilityId dis) <$> (liftIO . (`getsEntities` elems) =<< users)
      mapM_ (liftIO . notifyAboutDisruption dis ms mf) subscribedUsers
    notifyAboutEventIfNecessary DisruptionEvent {} = return ()

    userWatchingFacility :: FacilityId -> [User] -> [User]
    userWatchingFacility fid = filter (elem fid . uWatchingFacilities)

disruptionsStatePipe :: Disruptions -> Pipe DisruptionEvent Disruptions Elescore ()
disruptionsStatePipe = go
  where
    go st = do
      dev <- await
      let st' = applyEvent st dev
      yield st'
      go st'

disruptionHistoryPipe :: History -> Pipe DisruptionEvent History Elescore ()
disruptionHistoryPipe = go
  where
    go h = do
      dev <- await
      let h' = applyEventToHistory h dev
      yield h'
      go h'

stationFetchingPipe :: Int -> Pipe Facility Station Elescore ()
stationFetchingPipe delay = do
  sc <- lift stationCache
  forever $ do
    f <- await
    ms <- liftIO (getStation sc $ fStationId f)
    maybe (fetchRemoteStation f) (updateFacility f) ms

  where
    updateFacility f s = yield (s {sFacilities = insertMap (fId f) f (sFacilities s)})
    fetchRemoteStation f = do
      waitSeconds delay
      let sid = unStationId . fStationId $ f
      ms <- lift $ fetchStation sid
      liftIO . putStrLn $ "Fetched remote station " <> tshow sid
      either print (updateFacility f . toStation) ms

ioRefConsumer :: IORef a -> Consumer a Elescore ()
ioRefConsumer ref = forever $ await >>= liftIO . writeIORef ref

stationCacheConsumer :: Consumer Station Elescore ()
stationCacheConsumer = do
  sc <- lift stationCache
  forever (await >>= liftIO . putStation sc)

waitSeconds :: MonadIO m => Int -> m ()
waitSeconds = liftIO . threadDelay . (* 1000000)
