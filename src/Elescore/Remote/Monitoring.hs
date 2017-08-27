{-# LANGUAGE TemplateHaskell #-}

module Elescore.Remote.Monitoring
  ( Disruptions
  , Change(..)
  , DisruptionEvent(..)
  , mkDisruptionEvent
  , replayEvents
  , applyEvent
  , calculateChanges
  ) where

import           ClassyPrelude              hiding (getCurrentTime)
import           Data.Aeson.TH
import           Data.DateTime
import           Data.Map                   (elems, filterWithKey)
import           Data.UUID
import           Data.UUID.V4               (nextRandom)

import           Elescore.Disruptions.Types (DisruptionId)
import           Elescore.Remote.Types

data Change
  = New
  | Updated
  | Resolved
  deriving (Show, Eq)

data DisruptionEvent = DisruptionEvent
  { devId         :: !UUID
  , devOccurredOn :: !DateTime
  , devDisruption :: !DisruptionData
  , devChangeType :: !Change
  }

mkDisruptionEvent :: DisruptionData -> Change -> IO DisruptionEvent
mkDisruptionEvent dis c = do
  uuid <- nextRandom
  currDate <- getCurrentTime
  return (DisruptionEvent uuid currDate dis c)

replayEvents :: [DisruptionEvent] -> Disruptions
replayEvents = foldl' applyEvent mempty

applyEvent :: Disruptions -> DisruptionEvent -> Disruptions
applyEvent ds (DisruptionEvent _ _ dis change) =
  case change of
    New      -> insertMap (disId dis) dis ds
    Updated  -> insertMap (disId dis) dis ds
    Resolved -> deleteMap (disId dis) ds

calculateChanges :: Disruptions -> Disruptions -> [(DisruptionData, Change)]
calculateChanges d1 d2 =
  let
    new = differenceMap d2 d1 -- contains all elements in d2 that do not exist in d1
    updated = filterWithKey (elemChanged d2) d1
    resolved = differenceMap d1 d2 -- contains all elements from d1 that do not exist in d2
  in
    toChangePair new New ++ toChangePair updated Updated ++ toChangePair resolved Resolved
  where
    toChangePair :: Map DisruptionId DisruptionData -> Change -> [(DisruptionData, Change)]
    toChangePair ds c = fmap (\dis -> (dis, c)) (elems ds)

    elemChanged :: (Ord k, Eq a) => Map k a -> k -> a -> Bool
    elemChanged m k a =
      case lookup k m of
        Nothing -> False
        Just a' -> a /= a'

deriveJSON defaultOptions ''DisruptionEvent
deriveJSON defaultOptions ''Change
