{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Elescore.Api.Types where

import           ClassyPrelude                  hiding (toLower)
import           Data.Aeson.TH
import           Data.Char                      (toLower)
import           Data.DateTime

import           Elescore.Domain.Station
import           Elescore.Domain.Types          (Facility (..), FacilityId,
                                                 FacilityType, Point,
                                                 Station (..), StationId)
import           Elescore.Projection.Disruption

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

fromDisruption :: StationRepo -> Disruption -> IO (Maybe DisruptionMarker)
fromDisruption sr Disruption {..} = do
  ms <- findById sr dstationId

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

deriveJSON defaultOptions { fieldLabelModifier = (\(x:xs) -> toLower x : xs) . drop 3 } ''DisruptionMarker
