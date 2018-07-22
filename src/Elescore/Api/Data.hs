{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Elescore.Api.Data
  ( DataApi
  , dataServer
  ) where

import           ClassyPrelude           hiding (Handler)
import           Data.Map                (elems)
import           Servant

import           Elescore.Api.Types
import           Elescore.Domain
import           Elescore.Domain.Station (findAll)

type DataApi =
       "disruptions" :> DisruptionApi
  :<|> "stations" :> Get '[JSON] [Station]

type DisruptionApi = "current" :> Get '[JSON] [UIDisruption]

dataServer :: IORef Disruptions -> StationRepo -> Server DataApi
dataServer dis sr =
  currentDisruptionsHandler dis sr
  :<|> listStationsHandler sr

currentDisruptionsHandler :: IORef Disruptions -> StationRepo -> Handler [UIDisruption]
currentDisruptionsHandler dis srepo = liftIO $ do
  diss <- elems <$> readIORef dis
  mapM (mkUIDisruption srepo) diss

listStationsHandler :: StationRepo -> Handler [Station]
listStationsHandler = findAll
