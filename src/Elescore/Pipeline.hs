{-# LANGUAGE RecordWildCards #-}

module Elescore.Pipeline
  ( elepipe
  ) where

import           ClassyPrelude                     hiding ((<>))
import           Control.Concurrent                (threadDelay)
import           Data.Aeson                        (encode)
import           Data.Monoid                       ((<>))
import           Pipes

import           Elescore.Common.EventLog
import           Elescore.Common.Types
import           Elescore.Disruptions.StationCache
import           Elescore.Remote
import           Elescore.Types

elepipe :: [DisruptionEvent] -> Elescore ()
elepipe devs = do
  disRef <- currDisruptionsRef
  dis <- liftIO (readIORef disRef)

  runEffect (each devs >-> disruptionsStatePipe dis >-> ioRefConsumer disRef)

  let disEventP    = disruptionProducer 10 >-> disruptionEventPipe dis >-> eventLogPipe >-> printPipe >-> disruptionsStatePipe dis >-> ioRefConsumer disRef
      facilityP    = facilityProducer 3600 >-> stationFetchingPipe 5 >-> stationCacheConsumer

  elepar [runEffect disEventP,
          runEffect facilityP]

disruptionProducer :: Int -> Producer Disruptions Elescore ()
disruptionProducer delay = forever $ do
  diss <- lift (runAPI fetchDisruptions)
  either print (yield . toDisruptions) diss
  waitSeconds delay
  where
    toDisruptions = foldr (liftA2 insertMap disFacilityId id) mempty

facilityProducer :: Int -> Producer Facility Elescore ()
facilityProducer delay = forever $ do
  fs <- lift (runAPI fetchFacilities)
  either print (mapM_ yield) fs
  waitSeconds delay

disruptionEventPipe :: Disruptions -> Pipe Disruptions DisruptionEvent Elescore ()
disruptionEventPipe = go
  where
    go st = do
      st' <- await
      let evs = map (uncurry mkDisruptionEvent) (calculateChanges st st')
      mapM_ yield evs
      go st'

printPipe :: Pipe DisruptionEvent DisruptionEvent Elescore ()
printPipe = forever $ do
  dev <- await
  logDisruptionEvent dev
  yield dev

  where
    logDisruptionEvent DisruptionEvent {..} =
      putStrLn $ "[" ++ tshow devChange ++ "] " ++ decodeUtf8 (toStrict $ encode devDisruption)

eventLogPipe :: Pipe DisruptionEvent DisruptionEvent Elescore ()
eventLogPipe = forever $ do
  dev <- await
  lift $ do
    fp <- opts optEventLog
    liftIO (appendLog fp dev)
  yield dev

disruptionsStatePipe :: Disruptions -> Pipe DisruptionEvent Disruptions Elescore ()
disruptionsStatePipe = go
  where
    go st = do
      dev <- await
      let st' = applyEvent st dev
      yield st'
      go st'

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
      ms <- lift . runAPI $ fetchStation sid
      liftIO . putStrLn $ "Fetched remote station " <> tshow sid
      either print (updateFacility f) ms

ioRefConsumer :: IORef a -> Consumer a Elescore ()
ioRefConsumer ref = forever $ await >>= liftIO . writeIORef ref

stationCacheConsumer :: Consumer Station Elescore ()
stationCacheConsumer = do
  sc <- lift stationCache
  forever (await >>= liftIO . putStation sc)

waitSeconds :: MonadIO m => Int -> m ()
waitSeconds = liftIO . threadDelay . (* 1000000)
