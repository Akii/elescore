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
                                               Progress (dpLastUpdatedOn))
import           Elescore.Disruptions.Types
import           Elescore.Remote.StationCache
import           Elescore.Remote.Types

type DataApi =
       "current-disruptions" :> Get '[JSON] [UI.Disruption]
  :<|> "disruption-history" :> QueryParam "page" Int :> Get '[JSON] [Progress]
  :<|> "stations" :> Get '[JSON] [Station]

dataServer :: IORef Disruptions -> StationCache -> IORef History -> Server DataApi
dataServer dis sc h =
  currentDisruptionsHandler dis sc
  :<|> historyHandler h
  :<|> listStationsHandler sc

currentDisruptionsHandler :: IORef Disruptions -> StationCache -> Handler [UI.Disruption]
currentDisruptionsHandler ds sc = do
  dss <- elems <$> readIORef ds
  mapM (liftIO . UI.mkUIDisruption sc) dss

historyHandler :: IORef History -> Maybe Int -> Handler [Progress]
historyHandler h p =
  let page = fromMaybe 0 p
  in take 500 . drop (page * 500) . sortOn (Down . dpLastUpdatedOn) . elems <$> readIORef h

listStationsHandler :: StationCache -> Handler [Station]
listStationsHandler = liftIO . fmap elems . getStations
