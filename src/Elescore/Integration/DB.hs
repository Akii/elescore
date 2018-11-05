module Elescore.Integration.DB
  ( DB
  , mkDBSource
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

mkDBSource :: MonadIO m => Store -> ApiKey -> Host -> Manager -> m (Source m DB)
mkDBSource store key host mgr = do
  disP <- disruptions store
  fP <- facilities store
  oEvs <- liftIO (readStream store)

  let oState = replayMkSeed dbObjectMonitor . fmap evPayload $ oEvs
      oIds = keys oState
      oP = objects store oEvs

  (output, input) <- liftIO (spawn unbounded)

  return Source
     { disruptionEvents = apiP 10 fetchDisruptedFacilities >-> disP
     , facilityEvents = apiP 3600 (fetchFacilities Nothing) >-> fP >-> P.tee (toOutput output)
     , objectEvents = fromInput input >-> P.map evPayload >-> stationFilterP oIds >-> fetchStationP 5 >-> oP}

  where
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

    apiP :: MonadIO m => Int -> API a -> Producer a m ()
    apiP delay action = forever $ do
      a <- liftIO $ runReaderT action (key, mgr, host)
      either print yield a
      waitSeconds delay

disruptions :: MonadIO m => Store -> m (Pipe [Facility] (PersistedEvent (DisruptionEvent DB)) m ())
disruptions store = do
  disEvs <- liftIO (readStream store)

  return $ P.map (fmap mkMDisruption)
    >-> monitorP dbDisruptionMonitor (replayMkSeed dbDisruptionMonitor . fmap evPayload $ disEvs)
    >-> eventStoreP store
    >-> P.concat
    >-> eachBefore disEvs
    >-> filterUnknownP

facilities :: MonadIO m => Store -> m (Pipe [Facility] (PersistedEvent (FacilityEvent DB)) m ())
facilities store = do
  fEvs <- liftIO (readStream store)

  return $ P.map (fmap mkMFacility)
    >-> monitorP dbFacilityMonitor (replayMkSeed dbFacilityMonitor . fmap evPayload $ fEvs)
    >-> eventStoreP store
    >-> P.concat
    >-> eachBefore fEvs

objects :: MonadIO m => Store -> [PersistedEvent (ObjectEvent DB)] -> Pipe Station (PersistedEvent (ObjectEvent DB)) m ()
objects store oEvs =
  P.map mkMObject
    >-> monitorSP dbObjectMonitor (replayMkSeed dbObjectMonitor . fmap evPayload $ oEvs)
    >-> eventStoreP store
    >-> P.concat
    >-> eachBefore oEvs

-- | Filters out FaSta API unknown facility states effectively
filterUnknownP :: Monad m => Pipe (PersistedEvent (DisruptionEvent a)) (PersistedEvent (DisruptionEvent a)) m ()
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
