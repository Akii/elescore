{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Elescore.Pipeline
  ( elepipe
  ) where

import           ClassyPrelude                hiding (for, getCurrentTime, try)
import           Control.Concurrent           (threadDelay)
import           Data.DateTime
import           Pipes
import           Pipes.Concurrent hiding (atomically)
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
  s <- store
  key <- apiKey
  host <- config cfgHost
  mgr <- reqManager

  dbSource <- mkDBSource s key host mgr
  bogSource <- mkBogSource s
  allSource <- combineSource dbSource bogSource

  elepar [runEffect (disruptionEvents allSource >-> projectionP dpRef applyDisruptionEvent),
          runEffect (facilityEvents allSource >-> P.map evPayload >-> projectionP facRef applyFacilityEvent),
          runEffect (objectEvents allSource >-> P.map evPayload >-> projectionP objRef applyObjectEvent),
          runDowntimeProjection dtRef dpRef]

combineSource :: Source Elescore a -> Source Elescore b -> Elescore (Source Elescore All)
combineSource s1 s2 = do
  env <- ask

  (out1, in1) <- liftIO (spawn unbounded)
  (out2, in2) <- liftIO (spawn unbounded)
  (out3, in3) <- liftIO (spawn unbounded)
  (out4, in4) <- liftIO (spawn unbounded)
  (out5, in5) <- liftIO (spawn unbounded)
  (out6, in6) <- liftIO (spawn unbounded)

  let s1All = fmap (const All) s1
      s2All = fmap (const All) s2

  liftIO $ do
    void . forkIO . runElescore env . runEffect $ disruptionEvents s1All >-> toOutput out1
    void . forkIO . runElescore env . runEffect $ disruptionEvents s2All >-> toOutput out2
    void . forkIO . runElescore env . runEffect $ facilityEvents s1All >-> toOutput out3
    void . forkIO . runElescore env . runEffect $ facilityEvents s2All >-> toOutput out4
    void . forkIO . runElescore env . runEffect $ objectEvents s1All >-> toOutput out5
    void . forkIO . runElescore env . runEffect $ objectEvents s2All >-> toOutput out6

  return Source { disruptionEvents = fromInput (in1 <> in2)
                , facilityEvents = fromInput (in3 <> in4)
                , objectEvents = fromInput (in5 <> in6)}

projectionP :: TVar a -> (ev -> a -> a) -> Consumer ev Elescore ()
projectionP ref f = for cat $ liftIO . atomically . modifyTVar' ref . f

runDowntimeProjection :: IORef SumOfDowntimes -> TVar DisruptionProjection -> Elescore ()
runDowntimeProjection dtRef dpRef = forever $ do
  currT <- liftIO getCurrentTime
  diss <- liftIO (readTVarIO dpRef)

  let minus1Day = addMinutes (-1440) currT
      minus30Days = addMinutes (-30 * 1440) currT
      dtimes = DT.sumOfDowntimes
        . DT.extractRange minus30Days minus1Day
        $ (DT.computeDowntimes currT (dpDisruptions diss) :: DT.Downtimes DT.Day)

  liftIO $ do
    writeIORef dtRef dtimes
    threadDelay (60 * 1000000)
