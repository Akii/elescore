{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Elescore.Api.Types where

import           ClassyPrelude        hiding (toLower)
import           Data.Aeson.TH
import           Data.Char            (toLower)
import           Data.DateTime

import           Elescore.IdTypes
import           Elescore.Integration
import           Elescore.Projection  hiding (Facility)
import qualified Elescore.Projection  as DT

data DisruptionMarker = DisruptionMarker
  { disId                  :: Int
  , disStationId           :: SomeObjectId
  , disStationName         :: Text
  , disFacilityId          :: SomeFacilityId
  , disFacilityType        :: FacilityType
  , disFacilityDescription :: Text
  , disReason              :: Maybe Text
  , disSince               :: DateTime
  , disGeoCoordinates      :: GeoLocation
  } deriving (Eq, Show)

data UIStation = UIStation
  { uisId         :: SomeObjectId
  , uisName       :: Text
  , uisFacilities :: Map SomeFacilityId UIFacility
  } deriving (Eq, Show)

data UIFacility = UIFacility
  { uifId             :: SomeFacilityId
  , uifStationId      :: Maybe SomeObjectId
  , uifType           :: FacilityType
  , uifDescription    :: Text
  , uifGeoCoordinates :: Maybe GeoLocation
  , uifDowntime       :: Integer
  } deriving (Eq, Show)

fromDisruption :: Objects -> Facilities -> Disruption -> Maybe DisruptionMarker
fromDisruption objs fs Disruption {..} = do
  facility <- lookup dFacilityId fs
  objectId <- fObjectId facility
  object <- lookup objectId objs
  geoCoord <- fGeoCoordinates facility
  return
    (DisruptionMarker
       dId
       objectId
       (oDescription object)
       dFacilityId
       (fType facility)
       (fDescription facility)
       (fmap tshow dReason)
       dOccurredOn
       geoCoord)

fromStation :: SumOfDowntimes -> Facilities -> DT.Object -> UIStation
fromStation sodt fs o =
  let uisId = DT.oId o
      uisName = DT.oDescription o
      uisFacilities = fromFacility sodt <$> filterMap ((==) (Just uisId) . fObjectId) fs
  in UIStation {..}

fromFacility :: SumOfDowntimes -> DT.Facility -> UIFacility
fromFacility sodt f =
  let dtime = fromMaybe 0 $ lookup (DT.fId f) sodt
  in mapFacility dtime f

mapFacility :: Integer -> DT.Facility -> UIFacility
mapFacility fDowntime =
  UIFacility <$> fId <*> fObjectId <*> fType <*> fDescription <*> fGeoCoordinates <*> pure fDowntime

deriveJSON defaultOptions { fieldLabelModifier = (\(x:xs) -> toLower x : xs) . drop 3 } ''DisruptionMarker
deriveJSON defaultOptions { fieldLabelModifier = (\(x:xs) -> toLower x : xs) . drop 3 } ''UIStation
deriveJSON defaultOptions { fieldLabelModifier = (\(x:xs) -> toLower x : xs) . drop 3 } ''UIFacility
