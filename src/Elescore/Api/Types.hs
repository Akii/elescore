{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Elescore.Api.Types where

import           ClassyPrelude        hiding (toLower)
import           Data.Aeson.TH
import           Data.Char            (toLower)
import           Data.DateTime
import           Servant.Pagination   hiding (defaultOptions)

import           Elescore.IdTypes
import           Elescore.Integration
import           Elescore.Projection  hiding (Facility)
import qualified Elescore.Projection  as DT

data UIOverallStats = UIOverallStats
  { uiosDisruptions       :: Int
  , uiosActiveDisruptions :: Int
  , uiosFacilities        :: Int
  , uiosObjects           :: Int
  }

data UIDisruptionPerDay = UIDisruptionPerDay
  { uidpdDay         :: Text
  , uidpdDisruptions :: Double
  }

data UIDisruption = UIDisruption
  { uidId           :: Int
  , uidFacilityId   :: FacilityId
  , uidFacilityName :: Text
  , uidObjectId     :: Maybe ObjectId
  , uidObjectName   :: Maybe Text
  , uidReason       :: Reason
  , uidOccurredOn   :: DateTime
  , uidUpdatedOn    :: Maybe DateTime
  , uidResolvedOn   :: Maybe DateTime
  , uidDuration     :: Integer
  } deriving (Generic, NFData, Show)

data UIMapMarker = UIMapMarker
  { uimId             :: Int
  , uimObjectId       :: ObjectId
  , uimObjectName     :: Text
  , uimFacilityId     :: FacilityId
  , uimFacilityType   :: FacilityType
  , uimFacilityName   :: Text
  , uimReason         :: Maybe Text
  , uimSince          :: DateTime
  , uimGeoCoordinates :: GeoLocation
  } deriving (Generic, NFData, Show)

data UIObjectSearchResult = UIObjectSearchResult
  { uiosrObject     :: UIObject
  , uiosrFacilities :: [UIFacility]
  } deriving (Generic, NFData, Eq, Show)

data UIObject = UIObject
  { uioId   :: ObjectId
  , uioName :: Text
  } deriving (Generic, NFData, Eq, Show)

data UIFacility = UIFacility
  { uifId          :: FacilityId
  , uifType        :: FacilityType
  , uifName        :: Text
  , uifIsDisrupted :: Bool
  } deriving (Generic, NFData, Eq, Show)

data UIFacilityDetails = UIFacilityDetails
  { uifdId          :: FacilityId
  , uifdType        :: FacilityType
  , uifdName        :: Text
  , uifdObject      :: Maybe UIObject
  , uifdDowntime    :: Integer
  , uifdIsDisrupted :: Bool
  } deriving (Generic, NFData, Eq, Show)

instance HasPagination UIDisruption "id" where
  type RangeType UIDisruption "id" = Int
  getFieldValue _ = uidId

instance HasPagination UIDisruption "occurredOn" where
  type RangeType UIDisruption "occurredOn" = DateTime
  getFieldValue _ = uidOccurredOn

instance HasPagination UIDisruption "updatedOn" where
  type RangeType UIDisruption "updatedOn" = Maybe DateTime
  getFieldValue _ = uidUpdatedOn

instance HasPagination UIDisruption "resolvedOn" where
  type RangeType UIDisruption "resolvedOn" = Maybe DateTime
  getFieldValue _ = uidResolvedOn

instance HasPagination UIDisruption "duration" where
  type RangeType UIDisruption "duration" = Integer
  getFieldValue _ = uidDuration

mkUIDisruptionPerDay :: (Text, Double) -> UIDisruptionPerDay
mkUIDisruptionPerDay = uncurry UIDisruptionPerDay

mkUIDisruption :: DateTime -> Objects -> Facilities -> Disruption -> Maybe UIDisruption
mkUIDisruption currentTime objects facilities Disruption {..} = do
  facility <- lookup dFacilityId facilities
  reason <- dReason

  let object = flip lookup objects =<< fObjectId facility
      objectId = oId <$> object
      objectName = oDescription <$> object
      duration = diffSeconds (fromMaybe currentTime dResolvedOn) dOccurredOn

  return (UIDisruption dId (fId facility) (fDescription facility) objectId objectName reason dOccurredOn dUpdatedOn dResolvedOn duration)

mkMapMarker :: Objects -> Facilities -> Disruption -> Maybe UIMapMarker
mkMapMarker objs fs Disruption {..} = do
  facility <- lookup dFacilityId fs
  objectId <- fObjectId facility
  object <- lookup objectId objs
  geoLocation <- fGeoCoordinates facility
  return
    (UIMapMarker
       dId
       objectId
       (oDescription object)
       dFacilityId
       (fType facility)
       (mapName facility)
       (fmap tshow dReason)
       dOccurredOn
       geoLocation)

mkObject :: DT.Object -> UIObject
mkObject o =
  let uioId = DT.oId o
      uioName = DT.oDescription o
  in UIObject {..}

mkFacility :: Bool -> DT.Facility -> UIFacility
mkFacility isDisrupted f@DT.Facility {..} =
  UIFacility fId fType (mapName f) isDisrupted

mkFacilityDetails :: Bool -> Integer -> Maybe DT.Object -> DT.Facility -> UIFacilityDetails
mkFacilityDetails isDisrupted downtime maybeObject f@DT.Facility {..} =
  UIFacilityDetails fId fType (mapName f) (fmap mkObject maybeObject) downtime isDisrupted

mapName :: DT.Facility -> Text
mapName f =
  case fDescription f of
    "Unknown" -> getFacilityId (fId f)
    a         -> a

deriveJSON defaultOptions { fieldLabelModifier = (\(x:xs) -> toLower x : xs) . drop 4 } ''UIOverallStats
deriveJSON defaultOptions { fieldLabelModifier = (\(x:xs) -> toLower x : xs) . drop 5 } ''UIDisruptionPerDay
deriveJSON defaultOptions { fieldLabelModifier = (\(x:xs) -> toLower x : xs) . drop 3 } ''UIDisruption
deriveJSON defaultOptions { fieldLabelModifier = (\(x:xs) -> toLower x : xs) . drop 3 } ''UIMapMarker
deriveJSON defaultOptions { fieldLabelModifier = (\(x:xs) -> toLower x : xs) . drop 3 } ''UIObject
deriveJSON defaultOptions { fieldLabelModifier = (\(x:xs) -> toLower x : xs) . drop 3 } ''UIFacility
deriveJSON defaultOptions { fieldLabelModifier = (\(x:xs) -> toLower x : xs) . drop 5 } ''UIObjectSearchResult
deriveJSON defaultOptions { fieldLabelModifier = (\(x:xs) -> toLower x : xs) . drop 4 } ''UIFacilityDetails
