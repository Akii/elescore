{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Elescore.Types where

import           ClassyPrelude
import           Network.HTTP.Client          (Manager)
import           Options.Applicative

import           Elescore.Disruptions.Types
import           Elescore.Remote.StationCache
import           Elescore.Remote.Types
import           Elescore.Users.Types         (Users)

-- The Elescore Monad

data Env = Env
  { envOpts               :: !Opts
  , envRequestManager     :: !Manager
  , envCurrentDisruptions :: !(IORef Disruptions)
  , envStationCache       :: !StationCache
  , envUsers              :: !Users
  }

newtype Elescore a = Elescore
  { elescore :: ReaderT Env IO a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

mkEnv :: Opts -> Manager -> Disruptions -> StationCache -> Users -> IO Env
mkEnv o m d sc us = do
  disRef <- newIORef d
  return (Env o m disRef sc us)

runElescore :: Env -> Elescore a -> IO a
runElescore env e = runReaderT (elescore e) env

elepar :: Monoid a => [Elescore a] -> Elescore a
elepar es = Elescore $ do
  env <- ask
  as <- liftIO $ mapM (async . runElescore env) es
  mconcat <$> mapM waitAsync as

apiKey :: Elescore ApiKey
apiKey = Elescore $ asks (fromString . optApiKey . envOpts)

reqManager :: Elescore Manager
reqManager = Elescore $ asks envRequestManager

currDisruptionsRef :: Elescore (IORef Disruptions)
currDisruptionsRef = Elescore $ asks envCurrentDisruptions

stationCache :: Elescore StationCache
stationCache = Elescore $ asks envStationCache

stations :: Elescore Stations
stations = liftIO . getStations =<< stationCache

users :: Elescore Users
users = Elescore $ asks envUsers

opts :: (Opts -> a) -> Elescore a
opts f = Elescore $ asks (f . envOpts)

-- CMD line options

data Opts = Opts
    { optHost         :: !String
    , optApiKey       :: !String
    , optEventLog     :: !String
    , optStationCache :: !String
    , optUserRepo     :: !String
    , optPort         :: !Int
    , optStaticDir    :: !String
    } deriving (Eq, Show)

parseOptions :: IO Opts
parseOptions = execParser (info (helper <*> optsParser) desc)
  where
    desc = header "Elescore - Elevator and escalator disruption scoring service" <> fullDesc

    optsParser :: Parser Opts
    optsParser = Opts
        <$> (strOption $
                long "host"
                <> metavar "HOSTNAME"
                <> help "DB Open Data API host"
                <> value "api.deutschebahn.com")

        <*> (strOption $
                long "api-key"
                <> metavar "APIKEY"
                <> help "Api key for authentication")

        <*> (strOption $
                long "event-log"
                <> metavar "EVENTLOG"
                <> help "File where the disruption events are appended to"
                <> value "./event.log")

        <*> (strOption $
                long "station-cache"
                <> metavar "STATIONCACHE"
                <> help "File where the station queries are cached"
                <> value "./station.cache")

        <*> (strOption $
                long "user-repo"
                <> metavar "USERREPO"
                <> help "File where the users are saved"
                <> value "./users.repo")

        <*> (option auto $
                long "port"
                <> metavar "PORT"
                <> help "Port for the frontend services"
                <> value 8080)

        <*> (strOption $
                long "static-dir"
                <> metavar "STATICDIR"
                <> help "Directory of the static frontend resources"
                <> value "./static")
