{-# LANGUAGE TemplateHaskell #-}

module Elescore.Common.Types where

import           ClassyPrelude
import           Data.Aeson.Encoding
import           Data.Aeson.TH
import           Data.Aeson.Types                 (FromJSONKey (..),
                                                   ToJSONKey (..),
                                                   ToJSONKeyFunction (ToJSONKeyText))
import           Data.UUID
import           Data.UUID.V4                     (nextRandom)
import           Servant.Auth.Server.Internal.JWT

newtype DisruptionId = DisruptionId
  { getDisruptionId :: UUID
  } deriving (Eq, Ord, Show)

newtype UserId = UserId
  { getUserId :: UUID
  } deriving (Eq, Ord, Show)

newtype StationId = StationId
  { unStationId :: Int
  } deriving (Ord, Eq, Show)

newtype FacilityId = FacilityId
  { unFacilityId :: Int
  } deriving (Ord, Eq, Show)

data Station = Station
  { sId         :: !StationId
  , sName       :: !Text
  , sFacilities :: !(Map FacilityId Facility)
  } deriving (Eq, Show)

data Facility = Facility
  { fId             :: !FacilityId
  , fStationId      :: !StationId
  , fType           :: !FacilityType
  , fDescription    :: !(Maybe Text)
  , fGeoCoordinates :: !(Maybe Point)
  } deriving (Eq, Show)

data FacilityState
  = Active
  | Inactive
  | Unknown
  deriving (Eq, Show)

data FacilityType
  = Elevator
  | Escalator
  deriving (Eq, Show)

data Point = Point !Double !Double
  deriving (Eq, Show)

nextDisruptionId :: MonadIO m => m DisruptionId
nextDisruptionId = DisruptionId <$> liftIO nextRandom

nextUserId :: MonadIO m => m UserId
nextUserId = UserId <$> liftIO nextRandom

-- JSON instances

concat <$> mapM
  (deriveJSON defaultOptions {unwrapUnaryRecords = True})
  [''UserId, ''StationId, ''FacilityId, ''DisruptionId]

concat <$> mapM
  (deriveJSON defaultOptions)
  [''Station, ''Facility, ''FacilityState, ''FacilityType, ''Point]

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

instance FromJSONKey UserId where
  fromJSONKey = UserId <$> fromJSONKey

instance ToJSONKey UserId where
  toJSONKey = ToJSONKeyText f g
    where
      f (UserId i) = tshow i
      g = text . f

instance FromJWT UserId
instance ToJWT UserId
