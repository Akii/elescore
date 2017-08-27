module Elescore
  ( run
  , parseOptions
  ) where

import           ClassyPrelude
import           Network.HTTP.Client           (newManager)
import           Network.HTTP.Client.TLS       (tlsManagerSettings)
import           System.IO                     (BufferMode (LineBuffering),
                                                hSetBuffering, stdout)

import           Elescore.Api                  (eleapi)
import           Elescore.Pipeline             (elepipe)
import           Elescore.Remote.DisruptionLog (loadDisruptionEvents)
import           Elescore.Remote.Monitoring    (replayEvents)
import           Elescore.Remote.StationCache  (loadStations)
import           Elescore.Repository           (loadRepository)
import           Elescore.Types                (Opts (..), mkEnv, parseOptions,
                                                runElescore)

run :: Opts -> IO ()
run o = do
  hSetBuffering stdout LineBuffering
  putStrLn "Starting Elescore v1"

  devs <- loadDisruptionEvents (optEventLog o)
  sc <- loadStations (optStationCache o)
  usrs <- loadRepository (optUserRepo o)
  mgr <- newManager tlsManagerSettings
  env <- mkEnv o mgr (replayEvents devs) sc usrs

  _ <- async $ runElescore env eleapi
  runElescore env (elepipe devs)

