{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Elescore.Api.Data
  ( DataApi
  , dataServer
  ) where

import           ClassyPrelude                  hiding (Handler)
import           Data.IntMap                    (elems)
import           Servant

import           Elescore.Api.Types
import           Elescore.Domain                (Station)
import           Elescore.Domain.Station        (StationRepo, findAll)
import           Elescore.Projection.Disruption

type DataApi =
       "disruptions" :> DisruptionApi
  :<|> "stations" :> Get '[JSON] [Station]

type DisruptionApi =
       "marker" :> Get '[JSON] [DisruptionMarker]

dataServer :: IORef DisruptionProjection -> StationRepo -> Server DataApi
dataServer dis sr =
  disruptionMarkerHandler dis sr
  :<|> listStationsHandler sr

disruptionMarkerHandler :: IORef DisruptionProjection -> StationRepo -> Handler [DisruptionMarker]
disruptionMarkerHandler dis sr = do
  diss <- liftIO $ filter (isNothing . dresolvedOn) . elems . dpDisruptions <$> readIORef dis
  liftIO $ catMaybes <$> mapM (fromDisruption sr) diss

listStationsHandler :: StationRepo -> Handler [Station]
listStationsHandler = findAll
