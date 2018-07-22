module Main where

import Prelude
import System.Environment
import Elescore

main :: IO ()
main = do
  [cfgFile] <- getArgs
  run cfgFile
