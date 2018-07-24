{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Elescore.Projection.Disruption
  ( DisruptionProjection(..)
  , Disruption(..)
  , emptyDisruptionProjection
  , applyEvent
  ) where

import           ClassyPrelude
import           Data.Aeson.TH
import           Data.DateTime

import           Elescore.Domain.Types hiding (Disruption)

data DisruptionProjection = DP
  { dpActiveDisruptions :: Map FacilityId Int
  , dpDisruptions       :: IntMap Disruption
  }

data Disruption = Disruption
  { did         :: Int
  , dstationId  :: StationId
  , dfacilityId :: FacilityId
  , doccurredOn :: DateTime
  , dresolvedOn :: Maybe DateTime
  , dlog        :: [Progress]
  }

data Progress = Progress
  { pfacilityState :: FacilityState
  , preason        :: Maybe Text
  , poccurredOn    :: DateTime
  }

emptyDisruptionProjection :: DisruptionProjection
emptyDisruptionProjection = DP mempty mempty

applyEvent :: DisruptionProjection -> DisruptionEvent -> DisruptionProjection
applyEvent (DP active diss) ev =
  let existingDis = lookup (devFacilityId ev) active >>= flip lookup diss
      newDis = mkDisruption (length diss + 1) ev
      p = mkProgress ev
      dis = appendProgress p $ fromMaybe newDis existingDis
      diss' = insertMap (did dis) dis diss
      active' = housekeepActive dis
  in DP active' diss'
  where
    housekeepActive d =
      if isJust (dresolvedOn d)
        then deleteMap (dfacilityId d) active
        else insertMap (dfacilityId d) (did d) active

appendProgress :: Progress -> Disruption -> Disruption
appendProgress p d =
  case pfacilityState p of
    Unknown  -> appendedLog
    Inactive -> appendedLog
    Active   -> appendedLog { dresolvedOn = Just (poccurredOn p)}
  where
    appendedLog = d { dlog = dlog d ++ [p] }

mkDisruption :: Int -> DisruptionEvent -> Disruption
mkDisruption i ev =
  Disruption
    i
    (devStationId ev)
    (devFacilityId ev)
    (devOccurredOn ev)
    Nothing
    []

mkProgress :: DisruptionEvent -> Progress
mkProgress DisruptionEvent {..} =
  case devChange of
    New      -> Progress devFacilityState devReason devOccurredOn
    Updated  -> Progress devFacilityState devReason devOccurredOn
    Resolved -> Progress Active Nothing devOccurredOn

concat <$> mapM
  (deriveJSON defaultOptions {fieldLabelModifier = drop 1})
  [''Disruption, ''Progress]
