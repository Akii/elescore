{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Elescore.Disruptions.Types where

import           ClassyPrelude
import           Data.Aeson.Encoding
import           Data.Aeson.TH
import           Data.Aeson.Types
import           Data.DateTime

type Stations = Map StationId Station

newtype StationId =
  StationId { unStationId :: Int }
  deriving (Ord, Eq)

newtype FacilityId =
  FacilityId { unFacilityId :: Int }
  deriving (Ord, Eq)

newtype DisruptionId =
  DisruptionId { unDisruptionId :: Int }
  deriving (Ord, Eq)

data Station = Station
  { sId         :: !StationId
  , sName       :: !(Maybe Text)
  , sFacilities :: !(Map FacilityId Facility)
  }

data Facility = Facility
  { fId             :: !FacilityId
  , fStationId      :: !StationId
  , fType           :: !FacilityType
  , fGeoCoordinates :: !(Maybe Point)
  , fDescription    :: !(Maybe Text)
  }

data Disruption = Disruption
  { disId                 :: !DisruptionId
  , disStationId          :: !StationId
  , disFacilityId         :: !FacilityId
  , disFacilityType       :: !FacilityType
  , disFacilityState      :: !(Maybe FacilityState)
  , disReason             :: !(Maybe Text)
  , disDescription        :: !(Maybe Text)
  , disFurtherDescription :: !(Maybe Text)
  , disPlannedCompletion  :: !(Maybe DateTime)
  , disGeoCoordinates     :: !(Maybe Point)
  , disOutOfServiceOn     :: !(Maybe DateTime)
  , disOutOfServiceTo     :: !(Maybe DateTime)
  , disLastUpdate         :: !(Maybe DateTime)
  } deriving (Eq)

data FacilityType
  = Elevator
  | Escalator
  deriving (Show, Eq)

data FacilityState
  = Inactive
  | Active
  deriving (Show, Eq)

data Point =
  Point !Double !Double
  deriving (Show, Eq)

disruptionToStation :: Disruption -> Station
disruptionToStation d = Station (disStationId d) Nothing (singletonMap (disFacilityId d) $ disruptionToFacility d)

disruptionToFacility :: Disruption -> Facility
disruptionToFacility Disruption {..} =
  Facility disFacilityId disStationId disFacilityType disGeoCoordinates disDescription

instance Semigroup Station where
  s1 <> s2 = Station (sId s1) (sName s1 <|> sName s2) (unionWith ((<>)) (sFacilities s1) (sFacilities s2))

instance Semigroup Facility where
  f1 <> f2 =
    Facility
      (fId f1)
      (fStationId f1)
      (fType f1)
      (fGeoCoordinates f1 <|> fGeoCoordinates f2)
      (fDescription f1 <|> fDescription f2)

concat <$> mapM
  (deriveJSON defaultOptions {unwrapUnaryRecords = True})
  [''StationId, ''FacilityId, ''DisruptionId, ''Station, ''Facility, ''Disruption, ''FacilityType, ''FacilityState, ''Point]

instance FromJSONKey StationId where
  fromJSONKey = StationId <$> fromJSONKey

instance ToJSONKey StationId where
  toJSONKey = ToJSONKeyText f g
    where
      f (StationId i) = tshow i
      g = text . f

instance FromJSONKey FacilityId where
  fromJSONKey = FacilityId <$> fromJSONKey

instance ToJSONKey FacilityId where
  toJSONKey = ToJSONKeyText f g
    where
      f (FacilityId i) = tshow i
      g = text . f
