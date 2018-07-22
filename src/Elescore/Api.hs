{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Elescore.Api
  ( eleapi
  ) where

import           ClassyPrelude
import           Data.Proxy
import           Network.Wai.Handler.Warp  (run)
import           Servant.API
import           Servant.Server

import           Elescore.Api.Data
import           Elescore.Types

type API = "api" :> DataApi

eleapi :: Elescore ()
eleapi = do
  srepo <- stationRepo
  port <- config cfgPort
  diss <- disruptions

  liftIO . run port $ serve api (dataServer diss srepo)

  where
    api :: Proxy API
    api = Proxy
