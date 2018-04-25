{-# LANGUAGE TemplateHaskell #-}

module Elescore.Common.EventLog
  ( Event(..)
  , loadLog
  , loadLog'
  , appendLog
  ) where

import           ClassyPrelude         hiding (getCurrentTime)
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString.Char8 as BS
import           Data.DateTime
import           Data.UUID
import           Data.UUID.V4          (nextRandom)

data Event a = Ev
  { evId         :: UUID
  , evOccurredOn :: DateTime
  , evPayload    :: a
  }

loadLog :: FromJSON a => FilePath -> IO [a]
loadLog fp = fmap evPayload <$> loadLog' fp

loadLog' :: FromJSON a => FilePath -> IO [Event a]
loadLog' fp = do
  fileContents <- BS.readFile fp
  return $ catMaybes $ decodeStrict' <$> BS.lines fileContents

appendLog :: ToJSON a => FilePath -> a -> IO ()
appendLog fp a = do
  evid <- nextRandom
  evdate <- getCurrentTime
  BS.appendFile fp (toStrict $ encode (Ev evid evdate a) <> "\n" )

deriveJSON defaultOptions ''Event
