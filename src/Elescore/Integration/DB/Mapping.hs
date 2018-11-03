{-# LANGUAGE RecordWildCards #-}

module Elescore.Integration.DB.Mapping
  ( mkMDisruption
  , mkMFacility
  , mkMObject
  , fromObjectId
  ) where

import           ClassyPrelude

import           Elescore.IdTypes
import           Elescore.Integration.Common.Types
import           Elescore.Integration.DB.Types     hiding (FacilityState (..))

mkFacilityId :: Integer -> FacilityId DB
mkFacilityId i = FacilityId ("DB-" <> tshow i)

mkObjectId :: Integer -> ObjectId DB
mkObjectId i = ObjectId ("DB-" <> tshow i)

fromObjectId :: ObjectId DB -> Text
fromObjectId (ObjectId oid) = drop 3 oid

mkMDisruption :: Facility -> MDisruption
mkMDisruption Facility {..} =
  MD (mkFacilityId fequipmentnumber) (maybe NoneGiven translateReason fstateExplanation)

mkMFacility :: Facility -> MFacility
mkMFacility Facility {..} =
  MF
    (mkFacilityId fequipmentnumber)
    (Just (mkObjectId fstationnumber))
    (mapCoordinates fgeocoordX fgeocoordY)
    (mapFacilityType ftype)
    (fromMaybe "Unknown" fdescription)

mkMObject :: Station -> MObject
mkMObject Station {..} =
  MO (mkObjectId sstationnumber) sname

translateReason :: Text -> Reason
translateReason t =
  case t of
    "under construction"       -> UnderConstruction
    "monitoring disrupted"     -> MonitoringDisrupted
    "not available"            -> NotAvailable
    "monitoring not available" -> MonitoringNotAvailable
    "under maintenance"        -> UnderMaintenance
    a                          -> Unknown a

mapFacilityType :: Text -> FacilityType
mapFacilityType t =
  case t of
    "ELEVATOR" -> Elevator
    _          -> Escalator

mapCoordinates :: Maybe Double -> Maybe Double -> Maybe GeoLocation
mapCoordinates = liftA2 GeoLocation
