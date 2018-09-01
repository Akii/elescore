{-# LANGUAGE RecordWildCards #-}

module Elescore.Pipeline
  ( elepipe
  ) where

import           ClassyPrelude                  hiding (getCurrentTime, (<>))
import           Control.Concurrent             (threadDelay)
import           Data.DateTime
import           Data.Monoid                    ((<>))
import           Pipes
import qualified Pipes.Prelude as P

import           Elescore.Domain
import           Elescore.Domain.DisruptionLog  (DisruptionRepo (appendEvent, findAll))
import qualified Elescore.Domain.Station        as S
import qualified Elescore.Projection.Disruption as DP
import qualified Elescore.Projection.Downtime   as DT
import           Elescore.Remote                (fetchDisruptedFacilities,
                                                 fetchFacilities, fetchStation,
                                                 mapDisruption, mapFacility,
                                                 mapStation, runAPI)
import           Elescore.Types

elepipe :: Elescore (Async ())
elepipe = do
  drepo <- disruptionRepo
  dpRef <- disruptions
  dtRef <- downtimes
  devs <- findAll drepo

  runEffect $ each devs >-> P.filter ((/=) Unknown . devFacilityState) >-> disruptionProjectionPipe dpRef >-> ioRefConsumer dpRef

  let disEventP    = disruptedFacilitiesProducer 10
                     >-> disruptionEventPipe (replayEvents devs)
                     >-> eventLogPipe
                     >-> printPipe
                     >-> P.filter ((/=) Unknown . devFacilityState)
                     >-> disruptionProjectionPipe dpRef
                     >-> ioRefConsumer dpRef

      facilityP    = facilityProducer 3600
                     >-> stationFetchingPipe 5
                     >-> stationConsumer

  elepar [runEffect disEventP,
          runEffect facilityP,
          runDowntimeProjection dtRef dpRef]

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

disruptionProjectionPipe :: IORef DP.DisruptionProjection -> Pipe DisruptionEvent DP.DisruptionProjection Elescore ()
disruptionProjectionPipe ref = readIORef ref >>= go
  where
    go st = do
      dev <- await
      let st' = DP.applyEvent st dev
      yield st'
      go st'

runDowntimeProjection :: IORef DT.SumOfDowntimes -> IORef DP.DisruptionProjection -> Elescore ()
runDowntimeProjection dtRef dpRef = forever $ do
  currT <- liftIO getCurrentTime
  diss <- liftIO (readIORef dpRef)

  let minus1Day = addMinutes (-1440) currT
      minus30Days = addMinutes (-30 * 1440) currT
      dtimes = DT.sumOfDowntimes
        . DT.extractRange minus30Days minus1Day
        $ (DT.computeDowntimes currT (DP.dpDisruptions diss) :: DT.Downtimes DT.Day)

  liftIO $ do
    writeIORef dtRef dtimes
    waitSeconds 60

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
