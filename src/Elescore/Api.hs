{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Elescore.Api
  ( eleapi
  ) where

import           ClassyPrelude
import           Data.Proxy
import           Network.Wai.Handler.Warp            (run)
import           Servant.API
import           Servant.Server

import           Elescore.Api.Handler.Data
import           Elescore.Types                      (Elescore, Opts (..),
                                                      currDisruptionsRef,opts,
                                                      stationCache)

type API = "api" :> DataApi

eleapi :: Elescore ()
eleapi = do
  disRef <- currDisruptionsRef
  sc <- stationCache
  port <- opts optPort

  liftIO . run port $ serve api (dataServer disRef sc)

  where
    api :: Proxy API
    api = Proxy
