module Elescore.Users.Registration
  ( startRegistration
  , completeRegistration
  ) where

import           ClassyPrelude
import           Control.Monad.Except
import           Control.Monad.State
import           Crypto.Random
import qualified Data.ByteString.Base16 as Hex

import           Elescore.Users.Types

startRegistration :: UserName -> EmailAddress -> UserAction RegistrationToken
startRegistration u e = toUserAction "username or email taken" $ do
  regs <- get

  if member u (regsNames regs) || member e (regsEmails regs)
    then return Nothing
    else Just <$> (mkRegistrationToken >>= insertToken u e)

  where
    mkRegistrationToken = liftIO $ RegistrationToken . decodeUtf8 . Hex.encode <$> getRandomBytes 32

completeRegistration :: RegistrationToken -> HashedPassword -> UserAction User
completeRegistration t pw = toUserAction "unknown token" $ do
  regs <- get

  case lookup t (regsTokens regs) of
    Nothing    -> return Nothing
    Just (u,e) -> do
      removeToken t
      liftIO $ Just <$> mkUser u e pw

toUserAction ::  Text -> RegistrationAction (Maybe a) -> UserAction a
toUserAction t r = do
  regsVar <- asks envRegistrations
  (a, regs') <- liftIO (readTVarIO regsVar >>= runStateT r)
  case a of
    Nothing -> throwError t
    Just a' -> do
      liftIO (atomically . writeTVar regsVar $ regs')
      return a'

insertToken :: UserName -> EmailAddress -> RegistrationToken -> RegistrationAction RegistrationToken
insertToken u e t =
  modify'
    (\rs ->
       rs
       { regsNames = insertSet u (regsNames rs)
       , regsEmails = insertSet e (regsEmails rs)
       , regsTokens = insertMap t (u, e) (regsTokens rs)
       }) >>
  return t

removeToken :: RegistrationToken -> RegistrationAction ()
removeToken t = modify' (\rs -> rs { regsTokens = deleteMap t (regsTokens rs) })
