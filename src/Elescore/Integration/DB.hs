module Elescore.Integration.DB
  ( DB
  , mkDBSource
  ) where

import           ClassyPrelude                          hiding (forM)
import           Control.Concurrent                     (threadDelay)
import           Network.HTTP.Client                    (Manager)
import           Pipes
import           Pipes.Concurrent
import qualified Pipes.Prelude                          as P

import           Elescore.IdTypes
import           Elescore.Integration.Common.Monitoring
import           Elescore.Integration.Common.Types
import           Elescore.Integration.DB.Client
import           Elescore.Integration.DB.Mapping
import           Elescore.Integration.DB.Monitoring
import           Elescore.Integration.DB.Types

mkDBSource :: MonadIO m => [DisruptionEvent DB] -> [FacilityEvent DB] -> [ObjectEvent DB] -> ApiKey -> Host -> Manager -> IO (Source m DB)
mkDBSource dev fev oev key host mgr = do
  let dState = replayMkSeed dbDisruptionMonitor dev
      fState = replayMkSeed dbFacilityMonitor fev
      oState = replayMkSeed dbObjectMonitor oev
      oIds = keys oState

  (out1, in1) <- spawn unbounded

  runEffect $ each fev >-> toOutput out1

  return Source
     { disruptionEvents =
         apiP 10 fetchDisruptedFacilities >-> P.map (fmap mkMDisruption) >-> monitorP dbDisruptionMonitor dState >-> P.concat
     , facilityEvents =
         apiP 3600 (fetchFacilities Nothing) >-> P.map (fmap mkMFacility) >-> monitorP dbFacilityMonitor fState >-> P.concat >-> P.tee (toOutput out1)
     , objectEvents =
         fromInput in1 >-> stationFilterP oIds >-> fetchStationP 5 >-> P.map mkMObject >-> monitorSP dbObjectMonitor oState >-> P.concat
     }
  where
    apiP :: MonadIO m => Int -> API a -> Producer a m ()
    apiP delay action = forever $ do
      a <- liftIO $ runReaderT action (key, mgr, host)
      either print yield a
      waitSeconds delay

    stationFilterP :: MonadIO m => [ObjectId DB] -> Pipe (FacilityEvent DB) (ObjectId DB) m ()
    stationFilterP = go
      where
        go s = do
          ev <- await
          case ev of
            FacilityAssignedToObject _ oid -> unless (oid `elem` s) (yield oid >> go (oid : s))
            _                              -> return ()
          go s

    fetchStationP :: MonadIO m => Int -> Pipe (ObjectId DB) Station m ()
    fetchStationP delay = forever $ do
      oid <- fromObjectId <$> await
      a <- liftIO $ runReaderT (fetchStation oid) (key, mgr, host)
      either print yield a
      waitSeconds delay

waitSeconds :: MonadIO m => Int -> m ()
waitSeconds = liftIO . threadDelay . (* 1000000)
