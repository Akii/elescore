{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}

module Elescore.Integration.DB.Types where

import           ClassyPrelude
import           Data.Aeson
import           Data.Aeson.TH
import           Servant

import           Elescore.IdTypes
import           Elescore.Integration.Common.Types (FacilityType, GeoLocation,
                                                    Reason)

data MDisruption = MD
  { mdId     :: FacilityId
  , mdReason :: Reason
  } deriving (Eq)

data MFacility = MF
  { mfId          :: FacilityId
  , mfObjectId    :: Maybe ObjectId
  , mfGeoLocation :: Maybe GeoLocation
  , mfType        :: FacilityType
  , mfDescription :: Text
  } deriving (Eq)

data MObject = MO
  { moId          :: ObjectId
  , moDescription :: Text
  } deriving (Eq, Ord)

data Station = Station
  { sstationnumber :: Integer
  , sname          :: Text
  , sfacilities    :: Maybe [Facility]
  } deriving (Show)

data Facility = Facility
  { fequipmentnumber  :: Integer
  , ftype             :: Text
  , fdescription      :: Maybe Text
  , fgeocoordX        :: Maybe Double
  , fgeocoordY        :: Maybe Double
  , fstate            :: FacilityState
  , fstateExplanation :: Maybe Text
  , fstationnumber    :: Integer
  } deriving (Show)

data FacilityState
  = Active
  | Inactive
  | Unknown
  deriving (Eq, Show)

instance FromJSON FacilityState where
  parseJSON = withText "the state" $ \s -> return $
    case s of
      "ACTIVE"   -> Active
      "INACTIVE" -> Inactive
      _          -> Unknown

concat <$> mapM
  (deriveFromJSON defaultOptions { fieldLabelModifier = drop 1 })
  [''Station, ''Facility]

instance ToHttpApiData FacilityState where
  toQueryParam Active   = "ACTIVE"
  toQueryParam Inactive = "INACTIVE"
  toQueryParam Unknown  = "UNKNOWN"

instance ToHttpApiData [FacilityState] where
  toQueryParam = intercalate "," . fmap toQueryParam

newtype ApiKey =
  ApiKey Text
  deriving (Eq)

instance IsString ApiKey where
  fromString = ApiKey . pack

instance ToHttpApiData ApiKey where
  toHeader (ApiKey key) = encodeUtf8 ("Bearer " <> key)
  toQueryParam (ApiKey key) = key
