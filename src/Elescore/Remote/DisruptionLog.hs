module Elescore.Remote.DisruptionLog
  ( loadDisruptionEvents
  , appendDisruptionEvent
  ) where

import           ClassyPrelude
import           Data.Aeson            (FromJSON, ToJSON, decodeStrict', encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy  as LBS

import Elescore.Types
import Elescore.Remote.Monitoring

loadDisruptionEvents :: FilePath -> IO [DisruptionEvent]
loadDisruptionEvents = loadEL

appendDisruptionEvent :: DisruptionEvent -> Elescore ()
appendDisruptionEvent dev = do
  fp <- opts optEventLog
  liftIO (appendToEL fp dev)

appendToEL :: ToJSON a => FilePath -> a -> IO ()
appendToEL fp x = BS.appendFile fp (LBS.toStrict $ encode x <> "\n" )

loadEL :: FromJSON a => FilePath -> IO [a]
loadEL fp = do
  fileContents <- BS.readFile fp
  return $ catMaybes $ decodeStrict' <$> BS.lines fileContents
