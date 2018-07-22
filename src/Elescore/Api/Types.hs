{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Elescore.Api.Types where

import           ClassyPrelude
import           Data.Aeson.TH

import           Elescore.Domain.Station
import           Elescore.Domain.Types

data UIDisruption = UIDisruption
  { uidStationId      :: !StationId
  , uidFacilityId     :: !FacilityId
  , uidStationName    :: !(Maybe Text)
  , uidFacilityType   :: !(Maybe FacilityType)
  , uidFacilityDescr  :: !(Maybe Text)
  , uidGeoCoordinates :: !(Maybe Point)
  }

mkUIDisruption :: StationRepo -> Disruption -> IO UIDisruption
mkUIDisruption srepo Disruption {..} = do
  ms <- findById srepo disStationId

  let mf = lookup disFacilityId =<< fmap sFacilities ms
      uidStationId = disStationId
      uidStationName = fmap sName ms
      uidFacilityId = disFacilityId
      uidFacilityType = fmap fType mf
      uidFacilityDescr = join . fmap fDescription $ mf
      uidGeoCoordinates = join . fmap fGeoCoordinates $ mf

  return UIDisruption {..}

deriveToJSON defaultOptions { fieldLabelModifier = drop 3 } ''UIDisruption
