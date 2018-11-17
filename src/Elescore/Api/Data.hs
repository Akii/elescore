{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Elescore.Api.Data
  ( DataApi
  , dataServer
  ) where

import           ClassyPrelude      hiding (Handler)
import           Servant

import           Elescore.Api.Types

type DataApi =
       "disruptions" :> DisruptionApi
  :<|> "stations" :> Get '[JSON] [UIStation]

type DisruptionApi =
       "marker" :> Get '[JSON] [DisruptionMarker]

dataServer :: IORef [DisruptionMarker] -> IORef [UIStation] -> Server DataApi
dataServer marker stations =
  disruptionMarkerHandler marker
  :<|> listStationsHandler stations

disruptionMarkerHandler :: IORef [DisruptionMarker] -> Handler [DisruptionMarker]
disruptionMarkerHandler = liftIO . readIORef

listStationsHandler :: IORef [UIStation] -> Handler [UIStation]
listStationsHandler = liftIO . readIORef
