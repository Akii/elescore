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
import           Elescore.Domain.Station        (StationRepo, findAll)
import           Elescore.Projection.Disruption
import           Elescore.Projection.Downtime   (SumOfDowntimes)

type DataApi =
       "disruptions" :> DisruptionApi
  :<|> "stations" :> Get '[JSON] [Station]

type DisruptionApi =
       "marker" :> Get '[JSON] [DisruptionMarker]

dataServer :: IORef DisruptionProjection -> IORef SumOfDowntimes -> StationRepo -> Server DataApi
dataServer dis dt sr =
  disruptionMarkerHandler dis dt sr
  :<|> listStationsHandler dt sr

disruptionMarkerHandler :: IORef DisruptionProjection -> IORef SumOfDowntimes -> StationRepo -> Handler [DisruptionMarker]
disruptionMarkerHandler dis dt sr = do
  sodt <- liftIO (readIORef dt)
  diss <- liftIO $ filter (isNothing . dresolvedOn) . elems . dpDisruptions <$> readIORef dis
  liftIO $ catMaybes <$> mapM (fromDisruption sr sodt) diss

listStationsHandler :: IORef SumOfDowntimes -> StationRepo -> Handler [Station]
listStationsHandler dt sr = do
  sodt <- liftIO (readIORef dt)
  fmap (fromStation sodt) <$> findAll sr
