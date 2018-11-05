module Elescore.Integration.DB
  ( DB
  , mkDBSource
  ) where

import           ClassyPrelude                          hiding (forM)
import           Network.HTTP.Client                    (Manager)
import           Pipes
import           Pipes.Concurrent
import qualified Pipes.Prelude                          as P

import Database.SimpleEventStore
import           Elescore.IdTypes
import           Elescore.Integration.Common.Monitoring
import           Elescore.Integration.Common.Types
import           Elescore.Integration.DB.Client
import           Elescore.Integration.DB.Mapping
import           Elescore.Integration.DB.Monitoring
import           Elescore.Integration.DB.Types

mkDBSource :: MonadIO m => Connection -> ApiKey -> Host -> Manager -> IO (Source m DB)
mkDBSource conn key host mgr = do
  fev <-  fmap evPayload <$> readStream conn
  dState <- replayMkSeed dbDisruptionMonitor . fmap evPayload <$> readStream conn
  oState <- replayMkSeed dbObjectMonitor . fmap evPayload <$> readStream conn

  let oIds = keys oState
      fState = replayMkSeed dbFacilityMonitor fev

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
