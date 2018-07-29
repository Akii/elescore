{-# LANGUAGE ScopedTypeVariables #-}

module Elescore.Projection.Downtime
  ( Downtime
  , AggregatedDowntime
  , Downtimes
  , SumOfDowntimes
  , computeDowntimes
  , extractRange
  , sumOfDowntimes
  , toDay
  , toMonth
  , toYear
  , convertPIT
  ) where

import           ClassyPrelude                  hiding (Day)
import           Control.Lens
import           Data.DateTime                  hiding (toGregorian)
import           Data.Map                       (filterWithKey, elems)
import           Data.Time.Clock

import           Elescore.Domain                (FacilityId)
import           Elescore.Projection.Disruption

type Downtime = Integer
type AggregatedDowntime a = Map (PointInTime a) Downtime
type Downtimes a = Map FacilityId (AggregatedDowntime a)
type SumOfDowntimes = Map FacilityId Downtime

computeDowntimes :: forall a. Ord a => DateTime -> (DateTime -> PointInTime a) -> IntMap Disruption -> Downtimes a
computeDowntimes currT f = foldl' (flip apply) mempty
  where
    apply :: Disruption -> Downtimes a -> Downtimes a
    apply d dt =
      let disDuration = diffSeconds (fromMaybe currT $ dresolvedOn d) (doccurredOn d)
          pits = distributeDuration (doccurredOn d) disDuration
      in foldl' (\b (pit,dur) -> over (at (dfacilityId d) . non mempty) (insertWith (+) pit dur) b) dt pits

    distributeDuration :: DateTime -> Integer -> [(PointInTime a, Integer)]
    distributeDuration dt n
      | n <= 0 = []
      | n <= secondsUntilNextDay = [(f dt, n `div` 60)]
      | otherwise = (f dt, secondsUntilNextDay `div` 60) : distributeDuration (addSeconds secondsUntilNextDay dt) (n - secondsUntilNextDay)

      where
        secondsUntilNextDay :: Integer
        secondsUntilNextDay = 86400 - floor (fromIntegral (diffTimeToPicoseconds (utctDayTime dt)) * 1e-12 :: Double)

extractRange :: DateTime -> DateTime -> Downtimes a -> Downtimes a
extractRange start end = fmap (filterWithKey (\k _ -> inRange k))
  where
    inRange :: PointInTime a -> Bool
    inRange (PIT _ _ dt) = dt >= start && dt <= end

sumOfDowntimes :: Downtimes a -> SumOfDowntimes
sumOfDowntimes = fmap (sum . elems)

-- granularities
data Day = Day deriving (Eq, Ord, Show)
data Month = Month deriving (Eq, Ord, Show)
data Year = Year deriving (Eq, Ord, Show)

-- DateTime in PIT is just kept for converting between granularities
data PointInTime a = PIT a Text DateTime
  deriving (Show)

instance Eq a => Eq (PointInTime a) where
  PIT g1 t1 _ == PIT g2 t2 _ = g1 == g2 && t1 == t2

instance Eq a => Ord (PointInTime a) where
  PIT _ t1 _ <= PIT _ t2 _ = t1 <= t2

toDay :: DateTime -> PointInTime Day
toDay dt =
  let (year, month, day) = toGregorian (utctDay dt)
      formatted = mconcat [tshow year, "-", tshow month, "-", tshow day]
  in PIT Day formatted dt

toMonth :: DateTime -> PointInTime Month
toMonth dt =
  let (year, month, _) = toGregorian (utctDay dt)
      formatted = mconcat [tshow year, "-", tshow month]
  in PIT Month formatted dt

toYear :: DateTime -> PointInTime Year
toYear dt =
  let (year, _, _) = toGregorian (utctDay dt)
  in PIT Year (tshow year) dt

convertPIT :: (DateTime -> PointInTime b) -> PointInTime a -> PointInTime b
convertPIT f (PIT _ _ dt) = f dt
