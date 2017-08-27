{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Elescore.Api.Handler.User
  ( UnprotectedUserApi
  , ProtectedUserApi
  , unprotectedUserServer
  , protectedUserServer
  ) where

import           ClassyPrelude                       hiding (Handler)
import           Data.Either                         (isLeft)
import           Data.Map                            (elems)
import           Servant
import           Servant.Auth.Server
import           Servant.Auth.Server.SetCookieOrphan ()
import qualified Data.ByteString.Lazy.Char8    as LBS

import           Elescore.Disruptions.Types
import           Elescore.Notifications
import           Elescore.Repository
import           Elescore.Users.Registration
import           Elescore.Users.Types
import qualified Elescore.Api.Types as UI

type UnprotectedUserApi =
       "account" :> "register" :> ReqBody '[JSON] (Text, Text) :> Post '[JSON] ()
  :<|> "account" :> "complete-registration" :> ReqBody '[JSON] (Text, Text) :> Post '[JSON] UI.User
  :<|> "account" :> "login" :> ReqBody '[JSON] (Text,Text) :> Post '[JSON] UI.User

type ProtectedUserApi =
       "account" :> Get '[JSON] UI.User
  :<|> "watchlist" :> "add" :> ReqBody '[JSON] FacilityId :> Post '[JSON] (Set FacilityId)
  :<|> "watchlist" :> "remove" :> ReqBody '[JSON] FacilityId :> Post '[JSON] (Set FacilityId)

unprotectedUserServer :: Env -> JWTSettings -> Server UnprotectedUserApi
unprotectedUserServer env jwt =
       registerHandler env
  :<|> completeRegistrationHandler env jwt
  :<|> loginHandler env jwt

protectedUserServer :: Env -> JWTSettings -> UserId -> Server ProtectedUserApi
protectedUserServer env jwt uid =
       accountHandler env jwt uid
  :<|> watchlistAddHandler env uid
  :<|> watchlistRemoveHandler env uid

registerHandler :: Env -> (Text,Text) -> Handler ()
registerHandler env (u,e) = do
  res <- liftIO $ runUserAction env $ do
    un <- mkUserName u
    em <- mkEmailAddress e
    startRegistration un em

  case res of
    Left _  -> throwError err400
    Right t -> liftIO $ do
      putStrLn $ "[Registration] Name: " <> u <> " | Email: " <> e <> " | Token: " <> unRegistrationToken t
      sendUserRegistrationMail u e t

completeRegistrationHandler :: Env -> JWTSettings -> (Text, Text) -> Handler UI.User
completeRegistrationHandler env jwt (t,pw) = do
  res <- liftIO $ do
    hpw <- mkHashedPassword pw
    runUserAction env (completeRegistration (RegistrationToken t) hpw >>= \u -> saveUser u >> return u)
  when (isLeft res) (throwError err400)
  either (const $ throwError err400) (accountHandler env jwt . uId) res

loginHandler :: Env -> JWTSettings -> (Text,Text) -> Handler UI.User
loginHandler env jwt (ident,pw) = do
  ur <- liftIO $ findByNameOrEmail . elems <$> getEntities (envUsers env)
  unless (maybe (validateDummyPassword pw) (validatePassword pw . uPassword) ur) $ throwError err404

  case ur of
    Nothing -> throwError err404
    Just u -> accountHandler env jwt (uId u)

  where
    findByNameOrEmail :: [User] -> Maybe User
    findByNameOrEmail us =
          find ((==) (UserName ident) . uName) us
      <|> find ((==) (EmailAddress ident) . uEmail) us

accountHandler :: Env -> JWTSettings -> UserId -> Handler UI.User
accountHandler env jwt uid = withUser env uid (mkUIUser jwt)

watchlistAddHandler :: Env -> UserId -> FacilityId -> Handler (Set FacilityId)
watchlistAddHandler env uid fid = withUser env uid $ \u -> do
  let u' = u { uWatchingFacilities = insertSet fid (uWatchingFacilities u) }

  stations <- liftIO $ getsEntities (envStations env) elems
  unless (facilityExists stations) $ throwError err404
  res <- liftIO $ runUserAction env $ saveUser u'

  case res of
    Left err -> print err >> throwError err500
    Right _ -> return (uWatchingFacilities u')

  where
    facilityExists = any (member fid . sFacilities)

watchlistRemoveHandler :: Env -> UserId -> FacilityId -> Handler (Set FacilityId)
watchlistRemoveHandler env uid fid = withUser env uid $ \u -> do
  let u' = u { uWatchingFacilities = deleteSet fid (uWatchingFacilities u) }
  res <- liftIO $ runUserAction env $ saveUser (u { uWatchingFacilities = deleteSet fid (uWatchingFacilities u) })

  case res of
    Left err -> print err >> throwError err500
    Right _ -> return (uWatchingFacilities u')

withUser :: Env -> UserId -> (User -> Handler a) -> Handler a
withUser env uid f = do
  mu <- liftIO $ lookup uid <$> getEntities (envUsers env)
  maybe (throwError err404) f mu

mkUIUser :: JWTSettings -> User -> Handler UI.User
mkUIUser jwt u = do
  token <- liftIO $ makeJWT (uId u) jwt Nothing
  either (const $ throwError err500) (return . mkUIUserWithToken . pack . LBS.unpack) token

  where
    mkUIUserWithToken t = UI.User (uName u) (uWatchingFacilities u) t
