{-# LANGUAGE RecordWildCards #-}

module Elescore.Integration.Bogestra.Monitor
  ( bogDisruptionMonitor
  , bogFacilityMonitor
  ) where

import           ClassyPrelude

import           Elescore.IdTypes
import           Elescore.Integration.Bogestra.Types
import           Elescore.Integration.Common.Monitoring
import qualified Elescore.Integration.Common.Types as T
import           Elescore.Integration.Common.Types      hiding
                                                         (FacilityType (..))

type DisruptionMonitor = Monitor (FacilityId Bogestra) (FacilityId Bogestra, Bool) (DisruptionEvent Bogestra)
type DisruptionState = MonitorState (FacilityId Bogestra) (FacilityId Bogestra, Bool)

type FacilityMonitor = Monitor (FacilityId Bogestra) Elevator (FacilityEvent Bogestra)
type FacilityState = MonitorState (FacilityId Bogestra) Elevator

bogDisruptionMonitor :: DisruptionMonitor
bogDisruptionMonitor = Monitor fst applyEvent toDisruptionEvent
  where
    applyEvent :: DisruptionEvent Bogestra -> DisruptionState -> DisruptionState
    applyEvent ev = case ev of
      FacilityDisrupted fid _    -> insertMap fid (fid, True)
      DisruptionReasonUpdated {} -> id
      FacilityRestored fid       -> insertMap fid (fid, False)

    toDisruptionEvent :: Change (FacilityId Bogestra, Bool) -> EmitsEvents (DisruptionEvent Bogestra)
    toDisruptionEvent c =
      case c of
        New (eid, disr) ->
          when disr (emitEvent $ FacilityDisrupted eid NotAvailable)
        Updated (_, disr1) (eid, disr2) -> do
          when (disr1 && not disr2) (emitEvent $ FacilityRestored eid)
          when (not disr1 && disr2) (emitEvent $ FacilityDisrupted eid NotAvailable)
        Deleted (eid,disr) ->
          when disr (emitEvent $ FacilityRestored eid)

bogFacilityMonitor :: FacilityMonitor
bogFacilityMonitor = Monitor eId applyEvent toFacilityEvent
  where
    applyEvent :: FacilityEvent Bogestra -> FacilityState -> FacilityState
    applyEvent ev = case ev of
      FacilityIdentified fid _ desc       -> insertMap fid (Elevator fid Nothing desc False)
      FacilityTypeChanged {}              -> id
      FacilityAssignedToObject fid oid    -> adjustMap (\e -> e { eObjectId = Just oid }) fid
      FacilityDescriptionUpdated fid desc -> adjustMap (\e -> e { eDescription = desc }) fid
      FacilityLocated {}                  -> id
      FacilityAddressUpdated {}           -> id
      FacilityDeleted fid                 -> deleteMap fid

    toFacilityEvent :: Change Elevator -> EmitsEvents (FacilityEvent Bogestra)
    toFacilityEvent c = case c of
        New Elevator {..} -> do
          emitEvent (FacilityIdentified eId T.Elevator eDescription)
          forM_ eObjectId (emitEvent . FacilityAssignedToObject eId)
        Updated e1 e2 -> do
          when (eObjectId e1 /= eObjectId e2) $
            forM_ (eObjectId e2) (emitEvent . FacilityAssignedToObject (eId e2))
          when (eDescription e1 /= eDescription e2) $
            emitEvent (FacilityDescriptionUpdated (eId e2) (eDescription e2))
        Deleted Elevator {..} -> emitEvent (FacilityDeleted eId)
