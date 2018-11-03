module Elescore
  ( run
  ) where

import           ClassyPrelude
import           System.Signal
import           Prelude           (read)
import           System.IO         (BufferMode (LineBuffering), stdout)

import           Elescore.Api      (eleapi)
import           Elescore.Pipeline (elepipe)
import           Elescore.Types    (mkEnv, runElescore)

run :: FilePath -> IO ()
run cfgFile = do
  hSetBuffering stdout LineBuffering
  putStrLn "Starting Elescore v3"

  cfg <- read . unpack <$> readFileUtf8 cfgFile
  env <- mkEnv cfg

  api <- async $ runElescore env eleapi
  pipe <- runElescore env elepipe

  installHandler sigINT (const $ shutdown api pipe)
  installHandler sigTERM (const $ shutdown api pipe)

  void (waitCatch pipe)
  void (waitCatch api)

  where
    shutdown api pipe = cancel api >> cancel pipe
