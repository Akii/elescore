{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Elescore.Types where

import           ClassyPrelude                 hiding (log)
import           Control.Monad.Catch
import           Network.HTTP.Client           (Manager, newManager)
import           Network.HTTP.Client.TLS       (tlsManagerSettings)

import           Database.SimpleEventStore     (Store, mkStore)
import           Elescore.Integration.DB.Types (ApiKey (..))
import           Elescore.Projection           (DisruptionProjection,
                                                Facilities, Objects,
                                                SumOfDowntimes,
                                                emptyDisruptionProjection)

data Config = Config
    { cfgHost     :: String
    , cfgApiKey   :: String
    , cfgDatabase :: String
    , cfgPort     :: Int
    } deriving (Read, Eq, Show)

data Env = Env
  { envConfig         :: Config
  , envRequestManager :: Manager
  , envStore          :: Store
  , envDisruptions    :: IORef DisruptionProjection
  , envDowntimes      :: IORef SumOfDowntimes
  , envObjects        :: IORef Objects
  , envFacilities     :: IORef Facilities
  }

newtype Elescore a = Elescore
  { elescore :: ReaderT Env IO a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadReader Env
             , MonadIO
             , MonadThrow
             , MonadCatch
             , MonadMask
             )

mkEnv :: Config -> IO Env
mkEnv c = do
  mgr <- newManager tlsManagerSettings
  conn <- mkStore (cfgDatabase c)
  diss <- newIORef emptyDisruptionProjection
  dtimes <- newIORef mempty
  objs <- newIORef mempty
  fs <- newIORef mempty
  return $ Env c mgr conn diss dtimes objs fs

runElescore :: Env -> Elescore a -> IO a
runElescore env e = runReaderT (elescore e) env

elepar :: Monoid a => [Elescore a] -> Elescore (Async a)
elepar es = do
  env <- ask
  liftIO . async $ mconcat <$> mapConcurrently (runElescore env) es

apiKey :: Elescore ApiKey
apiKey = Elescore $ asks (fromString . cfgApiKey . envConfig)

reqManager :: Elescore Manager
reqManager = Elescore $ asks envRequestManager

store :: Elescore Store
store = Elescore $ asks envStore

disruptions :: Elescore (IORef DisruptionProjection)
disruptions = Elescore $ asks envDisruptions

downtimes :: Elescore (IORef SumOfDowntimes)
downtimes = Elescore $ asks envDowntimes

objects :: Elescore (IORef Objects)
objects = Elescore $ asks envObjects

facilities :: Elescore (IORef Facilities)
facilities = Elescore $ asks envFacilities

config :: (Config -> a) -> Elescore a
config f = Elescore $ asks (f . envConfig)
