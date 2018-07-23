{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Elescore.Domain.Monitoring
  ( Disruptions
  , replayEvents
  , applyEvent
  , replayDisruptions
  , calculateChanges
  ) where

import           ClassyPrelude
import           Data.Map              (elems, filterWithKey)

import           Elescore.Domain.Types

type Disruptions = Map FacilityId Disruption

replayEvents :: [DisruptionEvent] -> Disruptions
replayEvents = foldl' applyEvent mempty

applyEvent :: Disruptions -> DisruptionEvent -> Disruptions
applyEvent ds ev =
  case devChange ev of
    New      -> insertMap (devFacilityId ev) (toDisruption ev) ds
    Updated  -> insertMap (devFacilityId ev) (toDisruption ev) ds
    Resolved -> deleteMap (devFacilityId ev) ds

replayDisruptions :: [Disruption] -> Disruptions
replayDisruptions = foldl' (\ds d -> insertMap (disFacilityId d) d ds) mempty

calculateChanges :: Disruptions -> Disruptions -> [(Disruption, Change)]
calculateChanges d1 d2 =
  let
    new = differenceMap d2 d1 -- contains all elements in d2 that do not exist in d1
    updated = filterWithKey (elemChanged d1) d2
    resolved = differenceMap d1 d2 -- contains all elements from d1 that do not exist in d2
  in
    toChangePair new New ++ toChangePair updated Updated ++ activeFacilityState (toChangePair resolved Resolved)
  where
    toChangePair :: Map FacilityId Disruption -> Change -> [(Disruption, Change)]
    toChangePair ds c = fmap ((,) <$> id <*> pure c) (elems ds)

    elemChanged :: (Ord k, Eq a) => Map k a -> k -> a -> Bool
    elemChanged m k a =
      case lookup k m of
        Nothing -> False
        Just a' -> a /= a'

    activeFacilityState :: [(Disruption, Change)] -> [(Disruption, Change)]
    activeFacilityState = fmap (\(d,c) -> (d { disFacilityState = Active, disReason = Nothing }, c))
