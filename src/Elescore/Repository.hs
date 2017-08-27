module Elescore.Repository
  ( Repository
  , loadRepository
  , saveRepository
  , getEntities
  , getsEntities
  , modifyEntities
  ) where

import           ClassyPrelude
import           Data.Aeson
import qualified Data.ByteString.Lazy       as LBS

data Repository a = Repository
  { rFile     :: !FilePath
  , rEntities :: !(TVar a)
  }

loadRepository :: (FromJSON a, Monoid a) => FilePath -> IO (Repository a)
loadRepository fp = do
  fileContents <- LBS.readFile fp
  var <- newTVarIO (fromMaybe mempty $ decode' fileContents)
  return (Repository fp var)

saveRepository :: ToJSON a => Repository a -> IO ()
saveRepository r = do
  entities <- readTVarIO (rEntities r)
  LBS.writeFile (rFile r) (encode entities)

getEntities :: Repository a -> IO a
getEntities r = readTVarIO (rEntities r)

getsEntities :: Repository a -> (a -> b) -> IO b
getsEntities r f = f <$> getEntities r

modifyEntities :: Repository a -> (a -> a) -> IO ()
modifyEntities r = atomically . modifyTVar' (rEntities r)
