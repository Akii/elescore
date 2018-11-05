module Elescore.Integration.Common.Monitoring
  ( MonitorState
  , EmitsEvents
  , Change(..)
  , Monitor(..)
  , mkSeed
  , replayMkSeed
  , emitEvent
  , monitorSP
  , monitorP
  , calculateChange
  , calculateChanges
  , waitSeconds
  ) where

import           ClassyPrelude
import           Control.Concurrent  (threadDelay)
import           Control.Monad.State
import           Data.Map            (elems, filterWithKey)
import           Pipes

type MonitorState a b = Map a b
type EmitsEvents a = State [a] ()

data Change a
  = New a
  | Updated a a
  | Deleted a
  deriving (Eq, Show)

data Monitor a b e = Monitor
  { identify :: b -> a
  , apply    :: e -> MonitorState a b -> MonitorState a b
  , toEvent  :: Change b -> EmitsEvents e
  }

mkSeed :: Ord a => Monitor a b e -> [b] -> MonitorState a b
mkSeed (Monitor f _ _) = foldl' (flip $ liftA2 insertMap f id) mempty

replayMkSeed :: Ord a => Monitor a b e -> [e] -> MonitorState a b
replayMkSeed m = foldl' (flip $ apply m) mempty

emitEvent :: a -> EmitsEvents a
emitEvent a = modify' (++ [a])

monitorSP :: (Monad m, Ord a, Eq b) => Monitor a b e -> MonitorState a b -> Pipe b [e] m ()
monitorSP m = go
  where
    go s = do
      b <- await
      let (s', evs) = calculateChange m s b
      yield evs
      go s'

monitorP :: (Monad m, Ord a, Eq b) => Monitor a b e -> MonitorState a b -> Pipe [b] [e] m ()
monitorP m = go
  where
    go s = do
      bs <- await
      let (s', evs) = calculateChanges m s bs
      yield evs
      go s'

calculateChange :: (Ord a, Eq b) => Monitor a b c -> MonitorState a b -> b -> (MonitorState a b, [c])
calculateChange monitor s1 b =
  let reducedState = filterWithKey (\k _ -> k == identify monitor b) s1
      (_, cs) = calculateChanges monitor reducedState [b]
  in (foldl' (flip $ apply monitor) s1 cs, cs)

calculateChanges :: (Ord a, Eq b) => Monitor a b c -> MonitorState a b -> [b] -> (MonitorState a b, [c])
calculateChanges monitor s1 bs =
  let
    s2 = mkSeed monitor bs
    new = New <$> elems (differenceMap s2 s1) -- contains all elements in s2 that do not exist in s1
    updated = catMaybes . elems $ intersectionWithMap elemChanged s1 s2
    deleted = Deleted <$> elems (differenceMap s1 s2) -- contains all elements from s1 that do not exist in s2
    events = concatMap (flip execState [] . toEvent monitor) (new ++ updated ++ deleted)
  in
    (foldl' (flip $ apply monitor) s1 events, events)
  where
    elemChanged :: Eq a => a -> a -> Maybe (Change a)
    elemChanged a1 a2 =
      if a1 /= a2
      then Just (Updated a1 a2)
      else Nothing

waitSeconds :: MonadIO m => Int -> m ()
waitSeconds = liftIO . threadDelay . (* 1000000)
