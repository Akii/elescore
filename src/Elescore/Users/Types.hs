{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Elescore.Users.Types where

import           ClassyPrelude
import           Control.Monad.Except
import           Control.Monad.State
import           Crypto.PasswordStore
import           Data.Aeson.Encoding              (text)
import           Data.Aeson.TH
import           Data.Aeson.Types                 (FromJSONKey (..),
                                                   ToJSONKey (..),
                                                   ToJSONKeyFunction (ToJSONKeyText))
import           Data.Attoparsec.ByteString.Char8
import           Data.Map                         (elems)
import           Data.Text                        (strip)
import           Data.UUID
import           Data.UUID.V4                     (nextRandom)
import           Servant.Auth.Server.Internal.JWT
import qualified Text.Email.Validate              as EmailV

import           Elescore.Disruptions.Types
import           Elescore.Remote.StationCache
import           Elescore.Repository

type Users = Repository (Map UserId User)

data Env = Env
  { envUsersFile     :: !FilePath
  , envUsers         :: !Users
  , envRegistrations :: !(TVar Registrations)
  , envStations      :: !StationCache
  }

newtype UserAction a = UserAction
  { unUserAction :: ExceptT Text (ReaderT Env IO) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadError Text)

mkEnv :: FilePath -> Users -> StationCache -> IO Env
mkEnv fp r sc = do
  users <- getEntities r
  regs <- newTVarIO (mkRegistrations (elems users))
  return (Env fp r regs sc)

runUserAction :: Env -> UserAction a -> IO (Either Text a)
runUserAction env = (`runReaderT` env) . runExceptT . unUserAction

newtype UserId = UserId
  { unUserId :: UUID
  } deriving (Ord, Eq)

newtype UserName = UserName
  { unUserName :: Text
  } deriving (Ord, Eq)

newtype EmailAddress = EmailAddress
  { unEmailAddress :: Text
  } deriving (Ord, Eq)

newtype HashedPassword =
  HashedPassword Text

newtype RegistrationToken = RegistrationToken
  { unRegistrationToken :: Text
  } deriving (Ord, Eq)

data User = User
  { uId                 :: !UserId
  , uName               :: !UserName
  , uEmail              :: !EmailAddress
  , uPassword           :: !HashedPassword
  , uWatchingFacilities :: !(Set FacilityId)
  }

saveUser :: User -> UserAction ()
saveUser u = do
  r <- asks envUsers
  liftIO $ do
    modifyEntities r (insertMap (uId u) u)
    saveRepository r

-- registrations

type RegistrationAction a = StateT Registrations IO a

data Registrations = Registrations
  { regsNames  :: !(Set UserName)
  , regsEmails :: !(Set EmailAddress)
  , regsTokens :: !(Map RegistrationToken (UserName, EmailAddress))
  }

mkRegistrations :: [User] -> Registrations
mkRegistrations xs =
  let names = foldr (insertSet . uName) mempty xs
      emails = foldr (insertSet . uEmail) mempty xs
  in Registrations names emails mempty

-- smart constructors

mkUser :: UserName -> EmailAddress -> HashedPassword -> IO User
mkUser n e pw = do
  uid <- UserId <$> nextRandom
  return (User uid n e pw mempty)

mkUserName :: Text -> UserAction UserName
mkUserName u =
  either (const . throwError $ "Invalid username") (return . UserName) $ parseOnly variableP (encodeUtf8 u)

  where
    variableP :: Parser Text
    variableP = do
      un <- strip . pack <$> many (digit <|> letter_ascii) <* endOfInput
      if (length un < 3 || length un > 16)
        then fail "Name too long or too short"
        else return un

mkEmailAddress :: Text -> UserAction EmailAddress
mkEmailAddress e = do
  let e' = strip e
  unless (EmailV.isValid $ encodeUtf8 e') $ throwError "Invalid email address"
  return (EmailAddress e')

mkHashedPassword :: Text -> IO HashedPassword
mkHashedPassword pw =
  HashedPassword . decodeUtf8 <$> makePassword (encodeUtf8 pw) 19

validatePassword :: Text -> HashedPassword -> Bool
validatePassword pw1 (HashedPassword pw2) = verifyPassword (encodeUtf8 pw1) (encodeUtf8 pw2)

validateDummyPassword :: Text -> Bool
validateDummyPassword t =
  let pw = HashedPassword "sha256|19|HwCSM7d9DwF0G+7PjDRNMQ==|UbOrEw/bsrZq2ai7uhYFjYNhZttGSIQAQjXQ+O6NjbE="
  in validatePassword ("prevents timing attacks" <> t) pw

concat <$> mapM
  (deriveJSON defaultOptions { unwrapUnaryRecords = True })
  [''UserId, ''UserName, ''EmailAddress, ''HashedPassword, ''User]

instance FromJSONKey UserId where
  fromJSONKey = UserId <$> fromJSONKey

instance ToJSONKey UserId where
  toJSONKey = ToJSONKeyText f g
    where
      f (UserId i) = tshow i
      g = text . f

instance FromJWT UserId
instance ToJWT UserId
