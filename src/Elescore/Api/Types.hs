{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Elescore.Api.Types where

import           ClassyPrelude
import           Data.Aeson.TH
import           Data.DateTime

import           Elescore.Disruptions.Types   hiding (Disruption)
import           Elescore.Remote.StationCache
import           Elescore.Remote.Types
import           Elescore.Users.Types         (UserName)

data User = User
  { uName      :: !UserName
  , uWatchlist :: !(Set FacilityId)
  , uToken     :: !(Text)
  }

data Disruption = Disruption
  { uidId             :: !DisruptionId
  , uidStationId      :: !StationId
  , uidStationName    :: !(Maybe Text)
  , uidFacilityId     :: !FacilityId
  , uidFacilityType   :: !FacilityType
  , uidFacilityDescr  :: !(Maybe Text)
  --, uidOccurredOn     :: DateTime -- todo fix this
  , uidUpdatedOn      :: !(Maybe DateTime)
  , uidGeoCoordinates :: !(Maybe Point)
  }

mkUIDisruption :: StationCache -> DisruptionData -> IO Disruption
mkUIDisruption sc DisruptionData {..} = do
  ms <- getStation sc disStationId

  let uidId = disId
      uidStationId = disStationId
      uidStationName = join $ fmap sName ms
      uidFacilityId = disEquipmentId
      uidFacilityType = disType
      uidFacilityDescr = join $ fmap fDescription =<< fmap (lookup disEquipmentId . sFacilities) ms
      uidUpdatedOn = disLastUpdate
      uidGeoCoordinates = join $ fmap fGeoCoordinates =<< fmap (lookup disEquipmentId . sFacilities) ms

  return Disruption {..}

deriveToJSON defaultOptions { fieldLabelModifier = drop 1 } ''User
deriveToJSON defaultOptions { fieldLabelModifier = drop 3 } ''Disruption
