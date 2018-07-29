{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Elescore.Api.Types where

import           ClassyPrelude                  hiding (toLower)
import           Data.Aeson.TH
import           Data.Char                      (toLower)
import           Data.DateTime

import           Elescore.Domain.Station
import qualified Elescore.Domain.Types as DT
import           Elescore.Domain.Types          (FacilityId,
                                                 FacilityType, Point, StationId)
import           Elescore.Projection.Disruption
import           Elescore.Projection.Downtime (SumOfDowntimes)

data DisruptionMarker = DisruptionMarker
  { disId                  :: Int
  , disStationId           :: StationId
  , disStationName         :: Text
  , disFacilityId          :: FacilityId
  , disFacilityType        :: FacilityType
  , disFacilityDescription :: Maybe Text
  , disReason              :: Maybe Text
  , disSince               :: DateTime
  , disGeoCoordinates      :: Point
  } deriving (Eq, Show)

data Station = Station
  { sId         :: StationId
  , sName       :: Text
  , sFacilities :: Map FacilityId Facility
  } deriving (Eq, Show)

data Facility = Facility
  { fId             :: FacilityId
  , fStationId      :: StationId
  , fType           :: FacilityType
  , fDescription    :: Maybe Text
  , fGeoCoordinates :: Maybe Point
  , fDowntime       :: Integer
  } deriving (Eq, Show)

fromDisruption :: StationRepo -> SumOfDowntimes -> Disruption -> IO (Maybe DisruptionMarker)
fromDisruption sr sodt Disruption {..} = do
  ms <- fmap (fromStation sodt) <$> findById sr dstationId

  let msn = sName <$> ms
      mf = lookup dfacilityId . sFacilities =<< ms
      mp = fGeoCoordinates =<< mf
      mft = fType <$> mf
      mfd = mf >>= fDescription
      mr = fmap preason . headMay . reverse $ dlog

  return $
    DisruptionMarker
      <$> pure did
      <*> pure dstationId
      <*> msn
      <*> pure dfacilityId
      <*> mft
      <*> pure mfd
      <*> mr
      <*> pure doccurredOn
      <*> mp

fromStation :: SumOfDowntimes -> DT.Station -> Station
fromStation sodt s =
  let sId = DT.sId s
      sName = DT.sName s
      sFacilities = fmap (fromFacility sodt) (DT.sFacilities s)
  in Station {..}

fromFacility :: SumOfDowntimes -> DT.Facility -> Facility
fromFacility sodt f =
  let dtime = fromMaybe 0 $ lookup (DT.fId f) sodt
  in mapFacility dtime f

mapFacility :: Integer -> DT.Facility -> Facility
mapFacility fDowntime DT.Facility {..} = Facility {..}

deriveJSON defaultOptions { fieldLabelModifier = (\(x:xs) -> toLower x : xs) . drop 3 } ''DisruptionMarker
deriveJSON defaultOptions { fieldLabelModifier = (\(x:xs) -> toLower x : xs) . drop 1 } ''Station
deriveJSON defaultOptions { fieldLabelModifier = (\(x:xs) -> toLower x : xs) . drop 1 } ''Facility
