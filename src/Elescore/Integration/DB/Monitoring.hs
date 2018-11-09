{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Elescore.Integration.DB.Monitoring
  ( dbDisruptionMonitor
  , dbFacilityMonitor
  , dbObjectMonitor
  ) where

import           ClassyPrelude

import           Elescore.IdTypes
import           Elescore.Integration.Common.Monitoring
import           Elescore.Integration.Common.Types
import           Elescore.Integration.DB.Types

type DisruptionMonitor = Monitor FacilityId MDisruption (DisruptionEvent 'DB)
type Disruptions = MonitorState FacilityId MDisruption

type FacilityMonitor = Monitor FacilityId MFacility (FacilityEvent 'DB)
type Facilities = MonitorState FacilityId MFacility

type ObjectMonitor = Monitor ObjectId MObject (ObjectEvent 'DB)
type Objects = MonitorState ObjectId MObject

dbDisruptionMonitor :: DisruptionMonitor
dbDisruptionMonitor = Monitor {identify = mdId, apply = applyEvent, toEvent = mkDisruptionEvent}
  where
    mkDisruptionEvent :: Change MDisruption -> EmitsEvents (DisruptionEvent 'DB)
    mkDisruptionEvent c = case c of
        New MD {..} ->
          emitEvent (FacilityDisrupted mdId mdReason)
        Updated _ MD {..} ->
          emitEvent (DisruptionReasonUpdated mdId mdReason)
        Deleted d ->
          emitEvent $ FacilityRestored (mdId d)

    applyEvent :: DisruptionEvent 'DB -> Disruptions -> Disruptions
    applyEvent ev = case ev of
        FacilityDisrupted fid r       -> insertMap fid (MD fid r)
        DisruptionReasonUpdated fid r -> insertMap fid (MD fid r)
        FacilityRestored fid          -> deleteMap fid

dbFacilityMonitor :: FacilityMonitor
dbFacilityMonitor = Monitor {identify = mfId, apply = applyEvent, toEvent = mkFacilityEvent}
  where
    mkFacilityEvent :: Change MFacility -> EmitsEvents (FacilityEvent 'DB)
    mkFacilityEvent c = case c of
        New MF {..}   -> do
          emitEvent (FacilityIdentified mfId mfType mfDescription)
          forM_ mfObjectId (emitEvent . FacilityAssignedToObject mfId)
          forM_ mfGeoLocation (emitEvent . FacilityLocated mfId)
        Updated f1 f2 -> do
          when (mfObjectId f1 /= mfObjectId f2) $
            forM_ (mfObjectId f2) (emitEvent . FacilityAssignedToObject (mfId f2))
          when (mfGeoLocation f1 /= mfGeoLocation f2) $
            forM_ (mfGeoLocation f2) (emitEvent . FacilityLocated (mfId f2))
          when (mfType f1 /= mfType f2) (emitEvent $ FacilityTypeChanged (mfId f2) (mfType f2))
          when (mfDescription f1 /= mfDescription f2) (emitEvent $ FacilityDescriptionUpdated (mfId f2) (mfDescription f2))
        Deleted MF {..}    -> emitEvent (FacilityDeleted mfId)

    applyEvent :: FacilityEvent 'DB -> Facilities -> Facilities
    applyEvent ev = case ev of
        FacilityIdentified fid ftype desc -> insertMap fid (MF fid Nothing Nothing ftype desc)
        FacilityAssignedToObject fid objId -> adjustMap (\f -> f { mfObjectId = Just objId }) fid
        FacilityLocated fid geo -> adjustMap (\f -> f { mfGeoLocation = Just geo }) fid
        FacilityDescriptionUpdated fid desc -> adjustMap (\f -> f { mfDescription = desc }) fid
        FacilityAddressUpdated {} -> id
        FacilityTypeChanged fid ftype -> adjustMap (\f -> f { mfType = ftype }) fid
        FacilityDeleted fid -> deleteMap fid

dbObjectMonitor :: ObjectMonitor
dbObjectMonitor = Monitor {identify = moId, apply = applyEvent, toEvent = mkFacilityEvent}
  where
    mkFacilityEvent :: Change MObject -> EmitsEvents (ObjectEvent 'DB)
    mkFacilityEvent c = case c of
      New MO {..} -> emitEvent (ObjectIdentified moId moDescription)
      Updated _ MO {..} -> emitEvent (ObjectDescriptionUpdated moId moDescription)
      Deleted MO {..} -> emitEvent (ObjectDeleted moId)

    applyEvent :: ObjectEvent 'DB -> Objects -> Objects
    applyEvent ev = case ev of
      ObjectIdentified oid desc -> insertMap oid (MO oid desc)
      ObjectDescriptionUpdated oid desc -> adjustMap (\o -> o { moDescription = desc }) oid
      ObjectLocated {} -> id
      ObjectAddressUpdated {} -> id
      ObjectDeleted oid -> deleteMap oid


