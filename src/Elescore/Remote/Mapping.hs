{-# LANGUAGE RecordWildCards #-}

module Elescore.Remote.Mapping
  ( mapDisruption
  , mapStation
  , mapFacility
  ) where

import           ClassyPrelude

import           Elescore.Domain.Types
import qualified Elescore.Domain.Types as Domain
import qualified Elescore.Remote.Types as Remote

mapDisruption :: Remote.Facility -> Domain.Disruption
mapDisruption Remote.Facility {..} =
  Disruption
  { disStationId = StationId fstationnumber
  , disFacilityId = FacilityId fequipmentnumber
  , disFacilityState = mapFacilityState fstate
  , disReason = fstateExplanation
  }

mapStation :: Remote.Station -> Domain.Station
mapStation Remote.Station {..} =
  Station
  { sId = StationId sstationnumber
  , sName = sname
  , sFacilities =
      foldr
        (liftA2 insertMap fId id . mapFacility)
        mempty
        (fromMaybe [] sfacilities)
  }

mapFacility :: Remote.Facility -> Domain.Facility
mapFacility Remote.Facility {..} =
  Facility
  { fId = FacilityId fequipmentnumber
  , fStationId = StationId fstationnumber
  , fType = mapFacilityType ftype
  , fDescription = fdescription
  , fGeoCoordinates = mapCoordinates fgeocoordX fgeocoordY
  }

mapFacilityType :: Text -> FacilityType
mapFacilityType t =
  case t of
    "ELEVATOR" -> Elevator
    _          -> Escalator

mapFacilityState :: Remote.FacilityState -> FacilityState
mapFacilityState fs =
  case fs of
    Remote.Active   -> Active
    Remote.Inactive -> Inactive
    Remote.Unknown  -> Unknown

mapCoordinates :: Maybe Double -> Maybe Double -> Maybe Point
mapCoordinates = liftA2 Point
