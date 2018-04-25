{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Elescore.Api.Handler.Data
  ( DataApi
  , dataServer
  ) where

import           ClassyPrelude                     hiding (Handler)
import           Data.Map                          (elems)
import           Servant

import           Elescore.Api.Types
import           Elescore.Common.Types
import           Elescore.Disruptions.StationCache
import           Elescore.Remote.Types

type DataApi =
       "disruptions" :> DisruptionApi
  :<|> "stations" :> Get '[JSON] [Station]

type DisruptionApi = "current" :> Get '[JSON] [UIDisruption]

dataServer :: IORef Disruptions -> StationCache -> Server DataApi
dataServer dis sc =
  currentDisruptionsHandler dis sc
  :<|> listStationsHandler sc

currentDisruptionsHandler :: IORef Disruptions -> StationCache -> Handler [UIDisruption]
currentDisruptionsHandler ds sc = do
  dss <- elems <$> readIORef ds
  mapM (liftIO . mkUIDisruption sc) dss

listStationsHandler :: StationCache -> Handler [Station]
listStationsHandler = liftIO . fmap elems . getStations
