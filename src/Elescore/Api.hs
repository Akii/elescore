{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Elescore.Api
  ( eleapi
  , module Elescore.Projection.Disruption
  ) where

import           ClassyPrelude
import           Control.Concurrent             (forkIO, threadDelay)
import qualified Data.IntMap                    as IM
import qualified Data.Map                       as M
import           Data.Proxy
import           Network.Wai.Handler.Warp       (run)
import           Servant.API
import           Servant.Server

import           Elescore.Api.Data
import           Elescore.Api.Types
import           Elescore.Projection
import           Elescore.Projection.Disruption
import           Elescore.Types
import           Elescore.IdTypes

type API = "api" :> DataApi

eleapi :: Elescore ()
eleapi = do
  port <- config cfgPort
  diss <- disruptions
  dissPerDay <- disruptionsPerDay
  dt <- downtimes
  objR <- objects
  fcR <- facilities

  statsRef <- newIORef (UIOverallStats 0 0 0 0)
  disruptionsRef <- newIORef mempty
  markerRef <- newIORef mempty
  objectSearchRef <- newIORef mempty
  facilityDetailsRef <- newIORef mempty

  runFrontendCache statsRef disruptionsRef markerRef objectSearchRef facilityDetailsRef diss dt objR fcR

  liftIO . run port $ serve api (dataServer statsRef dissPerDay disruptionsRef markerRef objectSearchRef facilityDetailsRef)

  where
    api :: Proxy API
    api = Proxy

runFrontendCache
  :: MonadIO m
  => IORef UIOverallStats
  -> IORef [UIDisruption]
  -> IORef [UIMapMarker]
  -> IORef [UIObjectSearchResult]
  -> IORef (Map FacilityId UIFacilityDetails)
  -> IORef DisruptionProjection
  -> IORef SumOfDowntimes
  -> IORef Objects
  -> IORef Facilities
  -> m ()
runFrontendCache statsRef disruptionsRef markerRef searchResultRef facilityDetailsRef disProjRef sodRef objRef facRef = void . liftIO . forkIO . forever $ do
  currentTime <- getCurrentTime
  objs <- readIORef objRef
  sod <- readIORef sodRef
  fs <- readIORef facRef
  disProj <- readIORef disProjRef

  let diss = IM.elems (dpDisruptions disProj)
      activeDisruptions = dpActiveDisruptions disProj
      uiDisruptions = mapMaybe (mkUIDisruption currentTime objs fs) diss
      marker = mapMaybe (mkMapMarker objs fs) (filter (isNothing . dResolvedOn) diss)
      isDisrupted = isJust . flip lookup activeDisruptions
      searchResults = projectSearchResults isDisrupted objs fs
      facilityDetails = projectFacilityDetails isDisrupted sod objs fs
      overallStats = UIOverallStats (length uiDisruptions) (length activeDisruptions) (length fs) (length objs)

  void $ mapConcurrently id [writeIORef statsRef overallStats,
                             uiDisruptions `deepseq` writeIORef disruptionsRef uiDisruptions,
                             marker `deepseq` writeIORef markerRef marker,
                             searchResults `deepseq` writeIORef searchResultRef searchResults,
                             facilityDetails `deepseq` writeIORef facilityDetailsRef facilityDetails]

  threadDelay (15 * 1000000)

projectSearchResults :: (FacilityId -> Bool) -> Objects -> Facilities -> [UIObjectSearchResult]
projectSearchResults isDisrupted objs facs =
  let facilitiesByObjectId = M.fromListWith (++) $ map (fObjectId &&& pure) (M.elems facs)
  in fmap (\o -> UIObjectSearchResult o (fmap mkFacility' . fromMaybe [] . lookup (Just $ uioId o) $ facilitiesByObjectId)) (M.elems . fmap mkObject $ objs)
  where
    mkFacility' :: Facility -> UIFacility
    mkFacility' f = mkFacility (isDisrupted . fId $ f) f

projectFacilityDetails :: (FacilityId -> Bool) -> SumOfDowntimes -> Objects -> Facilities -> Map FacilityId UIFacilityDetails
projectFacilityDetails isDisrupted sumOfDowntimes objs =
  fmap mkFacilityDetails'

  where
    object f = flip lookup objs =<< fObjectId f

    mkFacilityDetails' :: Facility -> UIFacilityDetails
    mkFacilityDetails' f =
      mkFacilityDetails (isDisrupted . fId $ f) (M.findWithDefault 0 (fId f) sumOfDowntimes) (object f) f
