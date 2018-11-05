{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Elescore.Integration.Bogestra.Types where

import           ClassyPrelude
import           Data.Data
import           URI.ByteString

import           Database.SimpleEventStore hiding (Object)
import           Elescore.IdTypes
import           Elescore.Integration.Common.Types

data Bogestra deriving Data

instance HasStream (DisruptionEvent Bogestra) where
  getStream = "Disruptions.Bogestra"

instance HasStream (FacilityEvent Bogestra) where
  getStream = "Facilities.Bogestra"

instance HasStream (ObjectEvent Bogestra) where
  getStream = "Objects.Bogestra"

data Object
  = Bochum
  | Gelsenkirchen
  | Herne

data Elevator = Elevator
  { eId          :: FacilityId Bogestra
  , eObjectId    :: Maybe (ObjectId Bogestra)
  , eDescription :: Text
  , eIsDisrupted :: Bool
  } deriving (Eq, Show)

type Table = [TableRow]
data TableRow = TR
  { trStation         :: Text
  , trFacilityId      :: FacilityId Bogestra
  , trTravelDirection :: Maybe Text
  , trPlatform        :: Text
  , trDetailsLink     :: URI
  , trDisruptionLink  :: Maybe URI
  }

getObjectId :: Object -> ObjectId Bogestra
getObjectId Bochum        = ObjectId "BOG-Bochum"
getObjectId Gelsenkirchen = ObjectId "BOG-Gelsenkirchen"
getObjectId Herne         = ObjectId "BOG-Herne"

baseUrl :: IsString a => a
baseUrl = "https://www.bogestra.de"
