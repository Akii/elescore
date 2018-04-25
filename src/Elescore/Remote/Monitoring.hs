module Elescore.Remote.Monitoring
  ( Disruptions
  , Change(..)
  , DisruptionEvent(..)
  , mkDisruptionEvent
  , replayEvents
  , applyEvent
  , calculateChanges
  ) where

import           ClassyPrelude
import           Data.Map              (elems, filterWithKey)

import           Elescore.Common.Types
import           Elescore.Remote.Types

mkDisruptionEvent :: Disruption -> Change -> DisruptionEvent
mkDisruptionEvent = DisruptionEvent

replayEvents :: [DisruptionEvent] -> Disruptions
replayEvents = foldl' applyEvent mempty

applyEvent :: Disruptions -> DisruptionEvent -> Disruptions
applyEvent ds (DisruptionEvent d c) =
  case c  of
    New     -> insertMap (disFacilityId d) d ds
    Updated -> insertMap (disFacilityId d) d ds
    Deleted -> deleteMap (disFacilityId d) ds

calculateChanges :: Disruptions -> Disruptions -> [(Disruption, Change)]
calculateChanges d1 d2 =
  let
    new = differenceMap d2 d1 -- contains all elements in d2 that do not exist in d1
    updated = filterWithKey (elemChanged d2) d1
    resolved = differenceMap d1 d2 -- contains all elements from d1 that do not exist in d2
  in
    toChangePair new New ++ toChangePair updated Updated ++ toChangePair resolved Deleted
  where
    toChangePair :: Map FacilityId Disruption -> Change -> [(Disruption, Change)]
    toChangePair ds c = fmap (\dis -> (dis, c)) (elems ds)

    elemChanged :: (Ord k, Eq a) => Map k a -> k -> a -> Bool
    elemChanged m k a =
      case lookup k m of
        Nothing -> False
        Just a' -> a /= a'
