module Main where

import ClassyPrelude
import Elescore

main :: IO ()
main = parseOptions >>= run
