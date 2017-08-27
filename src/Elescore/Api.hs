{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

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
import           Servant.Utils.StaticFiles

import           Elescore.Api.Handler.Data
import           Elescore.Api.Handler.User
import           Elescore.Types                      (Elescore, Opts (..),
                                                      currDisruptionsRef, opts,
                                                      stationCache, users)
import           Elescore.Users.Types

type API auths =
       "api" :> Auth auths UserId :> ProtectedUserApi
  :<|> "api" :> UnprotectedUserApi
  :<|> "api" :> DataApi
  :<|> Raw

eleapi :: Elescore ()
eleapi = do
  disRef <- currDisruptionsRef
  sc <- stationCache
  port <- opts optPort
  dir <- opts optStaticDir
  userEnv <- mkUserEnv

  myKey <- liftIO generateKey

  let jwtCfg = defaultJWTSettings myKey
      cfg =  defaultCookieSettings :. jwtCfg :. EmptyContext

  liftIO $ run port $ serveWithContext api cfg (server jwtCfg disRef sc dir userEnv)

  where
    server jwt dis sc dir env =
           withAuth (protectedUserServer env jwt)
      :<|> unprotectedUserServer env jwt
      :<|> dataServer dis sc
      :<|> serveDirectoryFileServer dir

    withAuth s (Authenticated uid) = s uid
    withAuth _ _                   = throwAll err401

    api :: Proxy (API '[JWT])
    api = Proxy

mkUserEnv :: Elescore Env
mkUserEnv = do
  fp <- opts optUserRepo
  usrs <- users
  sc <- stationCache
  liftIO $ mkEnv fp usrs sc
