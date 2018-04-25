{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Elescore.Api
  ( eleapi
  ) where

import           ClassyPrelude
import           Data.Proxy
import           Network.Wai.Handler.Warp            (run)
import           Servant.API
import           Servant.Auth.Server
import           Servant.Auth.Server.SetCookieOrphan ()
import           Servant.Server

import           Elescore.Api.Handler.Data
import           Elescore.Api.Handler.User
import           Elescore.Types                      (Elescore, Opts (..),
                                                      currDisruptionsRef,opts,
                                                      stationCache, users)
import           Elescore.Users.Types
import Elescore.Common.Types

type API auths =
       "api" :> Auth auths UserId :> ProtectedUserApi
  :<|> "api" :> UnprotectedUserApi
  :<|> "api" :> DataApi

eleapi :: Elescore ()
eleapi = do
  disRef <- currDisruptionsRef
  sc <- stationCache
  port <- opts optPort
  userEnv <- mkUserEnv

  myKey <- liftIO generateKey

  let jwtCfg = defaultJWTSettings myKey
      cfg =  defaultCookieSettings :. jwtCfg :. EmptyContext

  liftIO $ run port $ serveWithContext api cfg (server jwtCfg disRef sc userEnv)

  where
    server jwt dis sc env =
           withAuth (protectedUserServer env jwt)
      :<|> unprotectedUserServer env jwt
      :<|> dataServer dis sc

    withAuth s (Authenticated uid) = s uid
    withAuth _ _                   = throwAll err401

    api :: Proxy (API '[JWT])
    api = Proxy

mkUserEnv :: Elescore Env
mkUserEnv = do
  fp <- opts optUserRepo
  usrs <- users
  sc <- stationCache
  liftIO $ mkEnv fp sc usrs
