{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Elescore.Api.Handler.Data
  ( DataApi
  , dataServer
  ) where

import           ClassyPrelude                hiding (Handler)
import           Data.Map                     (elems)
import           Servant

import qualified Elescore.Api.Types           as UI
import           Elescore.Disruptions.History (History,
                                               Progress (..), ProgressEvent(..))
import           Elescore.Disruptions.Types
import           Elescore.Remote.StationCache
import           Elescore.Remote.Types

type DataApi =
       "disruptions" :> DisruptionApi
  :<|> "stations" :> Get '[JSON] [Station]

type DisruptionApi =
       "current" :> Get '[JSON] [UI.Disruption]
  :<|> "history" :> QueryParam "page" Int :> Get '[JSON] [Progress]
  :<|> "history" :> "facility" :> Capture "facilityId" Int :> QueryParam "page" Int :> Get '[JSON] [Progress]

dataServer :: IORef Disruptions -> StationCache -> IORef History -> Server DataApi
dataServer dis sc h =
  (currentDisruptionsHandler dis sc
  :<|> historyHandler h
  :<|> facilityHistoryHandler h)
  :<|> listStationsHandler sc

currentDisruptionsHandler :: IORef Disruptions -> StationCache -> Handler [UI.Disruption]
currentDisruptionsHandler ds sc = do
  dss <- elems <$> readIORef ds
  mapM (liftIO . UI.mkUIDisruption sc) dss

historyHandler :: IORef History -> Maybe Int -> Handler [Progress]
historyHandler h p = paginate 500 p . sortHistory <$> readHistory h

facilityHistoryHandler :: IORef History -> Int -> Maybe Int -> Handler [Progress]
facilityHistoryHandler h fid p =
  paginate 500 p . sortHistory . filter ((==) fid . unFacilityId . dpFacilityId) <$> readHistory h

listStationsHandler :: StationCache -> Handler [Station]
listStationsHandler = liftIO . fmap elems . getStations

readHistory :: IORef History -> Handler [Progress]
readHistory = fmap elems . readIORef

sortHistory :: [Progress] -> [Progress]
sortHistory xs =
  let (open,resolved) = partition ((/=) Resolved . dpCurrentState) xs
  in sortOn (Down . dpLastUpdatedOn) open <> sortOn (Down . dpReportedOn) resolved

paginate :: Int -> Maybe Int -> [a] -> [a]
paginate perPage mpage =
  let page = fromMaybe 0 mpage
  in take perPage . drop (page * perPage)
