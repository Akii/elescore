module Elescore.Integration.Bogestra.Types where

import           ClassyPrelude
import           URI.ByteString

import           Elescore.IdTypes

data Object
  = Bochum
  | Gelsenkirchen
  | Herne

data Elevator = Elevator
  { eId          :: FacilityId
  , eObjectId    :: Maybe ObjectId
  , eDescription :: Text
  , eIsDisrupted :: Bool
  } deriving (Eq, Show)

type Table = [TableRow]
data TableRow = TR
  { trStation         :: Text
  , trFacilityId      :: FacilityId
  , trTravelDirection :: Maybe Text
  , trPlatform        :: Text
  , trDetailsLink     :: URI
  , trDisruptionLink  :: Maybe URI
  }

getObjectId :: Object -> ObjectId
getObjectId Bochum        = ObjectId "BOG-Bochum"
getObjectId Gelsenkirchen = ObjectId "BOG-Gelsenkirchen"
getObjectId Herne         = ObjectId "BOG-Herne"

baseUrl :: IsString a => a
baseUrl = "https://www.bogestra.de"
