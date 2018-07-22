{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Elescore.Pipeline
  ( elepipe
  ) where

import           ClassyPrelude                 hiding ((<>))
import           Control.Concurrent            (threadDelay)
import           Data.Monoid                   ((<>))
import           Pipes

import           Elescore.Domain
import           Elescore.Domain.DisruptionLog (DisruptionRepo (appendEvent, findAll))
import qualified Elescore.Domain.Station       as S
import           Elescore.Remote               (fetchDisruptedFacilities,
                                                fetchFacilities, fetchStation,
                                                mapDisruption, mapFacility,
                                                mapStation, runAPI)
import           Elescore.Types

elepipe :: Elescore (Async ())
elepipe = do
  drepo <- disruptionRepo
  disRef <- disruptions
  dis <- replayEvents <$> findAll drepo

  let disEventP    = disruptedFacilitiesProducer 10
                     >-> disruptionEventPipe dis
                     >-> eventLogPipe
                     >-> printPipe
                     >-> disruptionsStatePipe dis
                     >-> ioRefConsumer disRef

      facilityP    = facilityProducer 3600
                     >-> stationFetchingPipe 5
                     >-> stationConsumer

  elepar [runEffect disEventP,
          runEffect facilityP]

disruptedFacilitiesProducer :: Int -> Producer Disruptions Elescore ()
disruptedFacilitiesProducer delay = forever $ do
  fs <- lift (runAPI fetchDisruptedFacilities)
  either print (yield . replayDisruptions . fmap mapDisruption) fs
  waitSeconds delay

facilityProducer :: Int -> Producer Facility Elescore ()
facilityProducer delay = forever $ do
  fs <- lift (runAPI $ fetchFacilities Nothing)
  either print (mapM_ (yield . mapFacility)) fs
  waitSeconds delay

disruptionEventPipe :: Disruptions -> Pipe Disruptions (Disruption, Change) Elescore ()
disruptionEventPipe = go
  where
    go st = do
      st' <- await
      mapM_ yield (calculateChanges st st')
      go st'

eventLogPipe :: Pipe (Disruption, Change) DisruptionEvent Elescore ()
eventLogPipe = do
  drepo <- lift disruptionRepo
  forever $ do
    dev <- liftIO . uncurry mkDisruptionEvent =<< await
    liftIO (appendEvent drepo dev)
    yield dev

printPipe :: Pipe DisruptionEvent DisruptionEvent Elescore ()
printPipe = forever $ do
  dev <- await
  logDisruptionEvent dev
  yield dev

  where
    logDisruptionEvent DisruptionEvent {..} =
      lift . logInfo $ mconcat
        [ "["
        , tshow devChange
        , "] "
        , tshow (getStationId devStationId)
        , " | "
        , tshow (getFacilityId devFacilityId)
        , " | "
        , tshow (fromMaybe "" devReason)
        ]

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
  srepo <- lift stationRepo
  forever $ do
    f <- await
    ms <- liftIO (S.findById srepo $ fStationId f)
    maybe (fetchRemoteStation f) (updateFacility f) ms

  where
    updateFacility f s = yield (s {sFacilities = insertMap (fId f) f (sFacilities s)})
    fetchRemoteStation f = do
      waitSeconds delay
      let sid = getStationId . fStationId $ f
      ms <- lift $ runAPI (fetchStation sid)
      lift . logInfo $ "Fetched remote station " <> tshow sid
      either print (updateFacility f . mapStation) ms

ioRefConsumer :: IORef a -> Consumer a Elescore ()
ioRefConsumer ref = forever $ await >>= liftIO . writeIORef ref

stationConsumer :: Consumer Station Elescore ()
stationConsumer = do
  srepo <- lift stationRepo
  forever (await >>= liftIO . S.save srepo)

waitSeconds :: MonadIO m => Int -> m ()
waitSeconds = liftIO . threadDelay . (* 1000000)
