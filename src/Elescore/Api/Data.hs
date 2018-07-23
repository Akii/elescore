{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Elescore.Api.Data
  ( DataApi
  , dataServer
  ) where

import           ClassyPrelude                     hiding (Handler)
import           Data.IntMap                       (elems)
import           Servant

import           Elescore.Api.DisruptionProjection
import           Elescore.Domain                   (Station)
import           Elescore.Domain.Station           (StationRepo, findAll)

type DataApi =
       "disruptions" :> DisruptionApi
  :<|> "stations" :> Get '[JSON] [Station]

type DisruptionApi =
       "all" :> Get '[JSON] [Disruption]
  :<|> "current" :> Get '[JSON] [Disruption]

dataServer :: IORef DisruptionProjection -> StationRepo -> Server DataApi
dataServer dis sr =
  (allDisruptionsHandler dis :<|> currentDisruptionsHandler dis)
  :<|> listStationsHandler sr

allDisruptionsHandler :: IORef DisruptionProjection -> Handler [Disruption]
allDisruptionsHandler dis = liftIO $ elems . dpDisruptions <$> readIORef dis

currentDisruptionsHandler :: IORef DisruptionProjection -> Handler [Disruption]
currentDisruptionsHandler dis = liftIO $ filter (isNothing . dresolvedOn) . elems . dpDisruptions <$> readIORef dis

listStationsHandler :: StationRepo -> Handler [Station]
listStationsHandler = findAll
