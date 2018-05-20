module Elescore
  ( run
  , parseOptions
  ) where

import           ClassyPrelude
import           Network.HTTP.Client               (newManager)
import           Network.HTTP.Client.TLS           (tlsManagerSettings)
import           System.IO                         (BufferMode (LineBuffering),
                                                    stdout)

import           Elescore.Api                      (eleapi)
import           Elescore.Common.EventLog          (loadLog)
import           Elescore.Disruptions.StationCache (loadStations)
import           Elescore.Pipeline                 (elepipe)
import           Elescore.Remote.Monitoring        (replayEvents)
import           Elescore.Types                    (Opts (..), mkEnv,
                                                    parseOptions, runElescore)

run :: Opts -> IO ()
run o = do
  hSetBuffering stdout LineBuffering
  putStrLn "Starting Elescore v1"

  devs <- loadLog (optEventLog o)
  sc <- loadStations (optStationCache o)
  mgr <- newManager tlsManagerSettings
  env <- mkEnv o mgr (replayEvents devs) sc

  _ <- async $ runElescore env eleapi
  runElescore env (elepipe devs)
