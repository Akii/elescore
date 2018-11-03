{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Elescore.Pipeline
  ( elepipe
  ) where

import           ClassyPrelude                hiding (getCurrentTime, try, (<>))
import           Control.Concurrent           (threadDelay)
import           Control.Exception            (try)
import           Data.DateTime
import           Database.SQLite.Simple
import           Pipes
import qualified Pipes.Prelude                as P

import           Database.SimpleEventStore
import           Elescore.Integration
import           Elescore.Projection          (DisruptionProjection (dpDisruptions),
                                               SumOfDowntimes,
                                               applyDisruptionEvent,
                                               applyFacilityEvent,
                                               applyObjectEvent)
import qualified Elescore.Projection.Downtime as DT
import           Elescore.Types

elepipe :: Elescore (Async ())
elepipe = do
  dpRef <- disruptions
  facRef <- facilities
  objRef <- objects
  dtRef <- downtimes
  conn <- connection
  key <- apiKey
  host <- config cfgHost
  mgr <- reqManager

  dpEvs <- liftIO (readStream conn)
  fpEvs <- liftIO (readStream conn)
  opEvs <- liftIO (readStream conn)

  dbSource <- liftIO $ mkDBSource (fmap evPayload dpEvs) (fmap evPayload fpEvs) (fmap evPayload opEvs) key host mgr

  let disP = unknownFacilityStatusFilterP >-> projectionP dpRef applyDisruptionEvent
      facP = P.map evPayload >-> projectionP facRef applyFacilityEvent
      objP = P.map evPayload >-> projectionP objRef applyObjectEvent

  waitAsync =<< elepar [ runEffect (each dpEvs >-> disP)
                       , runEffect (each fpEvs >-> facP)
                       , runEffect (each opEvs >-> objP)]

  elepar [runEffect (disruptionEvents dbSource >-> eventStoreP conn >-> disP),
          runEffect (facilityEvents dbSource >-> eventStoreP conn >-> facP),
          runEffect (objectEvents dbSource >-> eventStoreP conn >-> objP),
          runDowntimeProjection dtRef dpRef]

projectionP :: IORef a -> (ev -> a -> a) -> Consumer ev Elescore ()
projectionP ref f = forever $ await >>= liftIO . modifyIORef' ref . f

-- | Filters out FaSta API unknown facility states effectively
unknownFacilityStatusFilterP :: Pipe (PersistedEvent (DisruptionEvent DB)) (PersistedEvent (DisruptionEvent DB)) Elescore ()
unknownFacilityStatusFilterP = forever $ do
  ev <- await
  case evPayload ev of
    FacilityDisrupted _ r       -> unless (unknownReason r) (yield ev)
    DisruptionReasonUpdated _ r -> unless (unknownReason r) (yield ev)
    _                           -> yield ev

  where
    unknownReason :: Reason -> Bool
    unknownReason MonitoringNotAvailable = True
    unknownReason MonitoringDisrupted    = True
    unknownReason _                      = False

runDowntimeProjection :: IORef SumOfDowntimes -> IORef DisruptionProjection -> Elescore ()
runDowntimeProjection dtRef dpRef = forever $ do
  currT <- liftIO getCurrentTime
  diss <- liftIO (readIORef dpRef)

  let minus1Day = addMinutes (-1440) currT
      minus30Days = addMinutes (-30 * 1440) currT
      dtimes = DT.sumOfDowntimes
        . DT.extractRange minus30Days minus1Day
        $ (DT.computeDowntimes currT (dpDisruptions diss) :: DT.Downtimes DT.Day)

  liftIO $ do
    writeIORef dtRef dtimes
    threadDelay (60 * 1000000)

eventStoreP :: forall a. (HasStream a, PersistableEvent a) => Connection -> Pipe a (PersistedEvent a) Elescore ()
eventStoreP conn = forever $ do
  ev <- await
  pev <- liftIO (try (append conn ev) :: IO (Either SQLError (PersistedEvent a)))
  either print yield pev
