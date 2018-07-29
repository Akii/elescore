{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Elescore.Api
  ( eleapi
  , module Elescore.Projection.Disruption
  ) where

import           ClassyPrelude
import           Data.Proxy
import           Network.Wai.Handler.Warp          (run)
import           Servant.API
import           Servant.Server

import           Elescore.Api.Data
import           Elescore.Projection.Disruption
import           Elescore.Types

type API = "api" :> DataApi

eleapi :: Elescore ()
eleapi = do
  srepo <- stationRepo
  port <- config cfgPort
  diss <- disruptions
  dt <- downtimes

  liftIO . run port $ serve api (dataServer diss dt srepo)

  where
    api :: Proxy API
    api = Proxy
