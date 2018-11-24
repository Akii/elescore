{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Elescore.Pipeline
  ( elepipe
  ) where

import           ClassyPrelude                hiding (for, getCurrentTime, try)
import           Control.Concurrent           (threadDelay)
import           Data.Coerce                  (coerce)
import           Data.DateTime
import           Pipes
import           Pipes.Concurrent             hiding (atomically)
import qualified Pipes.Prelude                as P

import           Database.SimpleEventStore
import           Elescore.Integration
import           Elescore.Projection          (DisruptionProjection (dpDisruptions),
                                               SumOfDowntimes,
                                               applyDisruptionEvent,
                                               applyFacilityEvent,
                                               applyObjectEvent,
                                               applyDisruptionPerDayEvent)
import qualified Elescore.Projection.Downtime as DT
import           Elescore.Types

elepipe :: Elescore (Async ())
elepipe = do
  dpRef <- disruptions
  dpPDRef <- disruptionsPerDay
  facRef <- facilities
  objRef <- objects
  dtRef <- downtimes
  s <- store
  key <- apiKey
  host <- config cfgHost
  mgr <- reqManager

  dbSource <- runDBSource s key host mgr
  bogSource <- runBogSource s
  let allSource = coerce dbSource <> coerce bogSource

  elepar [runEffect (fromInput (disruptionEvents allSource) >-> P.tee (projectionP dpPDRef applyDisruptionPerDayEvent) >-> projectionP dpRef applyDisruptionEvent),
          runEffect (fromInput (facilityEvents allSource) >-> P.map evPayload >-> projectionP facRef applyFacilityEvent),
          runEffect (fromInput (objectEvents allSource) >-> P.map evPayload >-> projectionP objRef applyObjectEvent),
          runDowntimeProjection dtRef dpRef]

projectionP :: IORef a -> (ev -> a -> a) -> Consumer ev Elescore ()
projectionP ref f = for cat $ liftIO . modifyIORef' ref . f

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
