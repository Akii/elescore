{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Elescore.Types where

import           ClassyPrelude                     hiding (log)
import           Control.Monad.Catch
import           Network.HTTP.Client               (Manager, newManager)
import           Network.HTTP.Client.TLS           (tlsManagerSettings)
import qualified System.Logger                     as Logger
import           System.Logger.Class               hiding (info)

import           Elescore.Projection.Disruption (DisruptionProjection,
                                                    emptyDisruptionProjection)
import           Elescore.Database
import           Elescore.Domain
import           Elescore.Remote.Types             (ApiKey (..))

data Config = Config
    { cfgHost     :: String
    , cfgApiKey   :: String
    , cfgDatabase :: String
    , cfgPort     :: Int
    } deriving (Read, Eq, Show)

data Env = Env
  { envConfig         :: Config
  , envLogger         :: Logger
  , envRequestManager :: Manager
  , envStationRepo    :: StationRepo
  , envDisruptionRepo :: DisruptionRepo
  , envDisruptions    :: IORef DisruptionProjection
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

instance MonadLogger Elescore where
  log l m = Elescore $ do
    lg <- asks envLogger
    liftIO $ Logger.log lg l m

mkEnv :: Config -> IO Env
mkEnv c = do
  mgr <- newManager tlsManagerSettings
  conn <- mkConnection (cfgDatabase c)
  diss <- newIORef emptyDisruptionProjection
  lg <- new (setOutput StdOut defSettings)
  return $ Env c lg mgr (mkStationRepo conn) (mkDisruptionRepo conn) diss

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

disruptionRepo :: Elescore DisruptionRepo
disruptionRepo = Elescore $ asks envDisruptionRepo

disruptions :: Elescore (IORef DisruptionProjection)
disruptions = Elescore $ asks envDisruptions

stationRepo :: Elescore StationRepo
stationRepo = Elescore $ asks envStationRepo

config :: (Config -> a) -> Elescore a
config f = Elescore $ asks (f . envConfig)

logInfo :: MonadLogger m => Text -> m ()
logInfo = log Logger.Info . Logger.msg
