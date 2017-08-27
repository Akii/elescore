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
import           Elescore.Disruptions.Types
import           Elescore.Remote.StationCache
import           Elescore.Remote.Types

type DataApi =
       "current-disruptions" :> Get '[JSON] [UI.Disruption]
  :<|> "stations" :> Get '[JSON] [Station]

dataServer :: IORef Disruptions -> StationCache -> Server DataApi
dataServer dis sc =
  currentDisruptionsHandler dis sc
  :<|> listStationsHandler sc

currentDisruptionsHandler :: IORef Disruptions -> StationCache -> Handler [UI.Disruption]
currentDisruptionsHandler ds sc = do
  dss <- elems <$> readIORef ds
  mapM (liftIO . UI.mkUIDisruption sc) dss

listStationsHandler :: StationCache -> Handler [Station]
listStationsHandler = liftIO . fmap elems . getStations
