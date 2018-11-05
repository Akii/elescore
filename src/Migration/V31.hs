module Migration.V31 where

import           ClassyPrelude             hiding (mapM_)
import           Control.Monad             (mapM_)

import           Database.SimpleEventStore
import           Elescore.IdTypes
import           Elescore.Integration

migrate :: FilePath -> IO ()
migrate db = do
  conn <- mkConnection db
  mapM_ (append conn) evs
  where
    evs :: [ObjectEvent Bogestra]
    evs =
      [ ObjectIdentified (ObjectId "BOG-Bochum") "Bochum"
      , ObjectIdentified (ObjectId "BOG-Gelsenkirchen") "Gelsenkirchen"
      , ObjectIdentified (ObjectId "BOG-Herne") "Herne"
      ]
