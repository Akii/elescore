module Main where

import Prelude
import Elescore

main :: IO ()
main = parseOptions >>= run
