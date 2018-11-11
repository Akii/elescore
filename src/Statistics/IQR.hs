module Statistics.IQR
  ( Sample
  , mkSample
  , singletonSample
  , insertSample
  , restrictSampleSize
  , withoutDuplicates
  , median
  , lowerQuartile
  , upperQuartile
  , iqr
  , lowerResiduals
  , upperResiduals
  , residuals
  , isResidual
  ) where

import           ClassyPrelude      hiding (head)
import           Data.List          (nub)
import           Data.List.NonEmpty (NonEmpty (..))
import           Prelude            (head, (!!))

newtype Sample = Sample
  { getSample :: [Double]
  } deriving (Show)

instance Semigroup Sample where
  sample1 <> sample2 = Sample . sort $ (getSample sample1 <> getSample sample2)

mkSample :: NonEmpty Double -> Sample
mkSample = Sample . sort . toList

singletonSample :: Double -> Sample
singletonSample = Sample . pure

insertSample :: Double -> Sample -> Sample
insertSample value (Sample sample) = mkSample (value :| sample)

withoutDuplicates :: Sample -> Sample
withoutDuplicates = Sample . nub . getSample

-- | This will keep the last n items of the sample.
restrictSampleSize :: Int -> Sample -> Sample
restrictSampleSize n
  | n < 1     = error "Cannot use sample size smaller than 1"
  | otherwise = Sample . reverse . take n . reverse . getSample

median :: Sample -> Double
median (Sample sample) =
  if odd sampleLength
    then sample !! middleIndex
    else (sample !! (middleIndex - 1) + sample !! middleIndex) / 2
  where
    sampleLength = length sample
    middleIndex = sampleLength `div` 2

lowerQuartile :: Sample -> Double
lowerQuartile (Sample sample) =
  if null lowerHalf
  then head sample
  else median (Sample lowerHalf)
  where
    lowerHalf = take (length sample `div` 2) sample

upperQuartile :: Sample -> Double
upperQuartile (Sample sample) =
  if null upperHalf
  then head (reverse sample)
  else median (Sample upperHalf)
  where
    upperHalf = drop (ceiling $ fromIntegral (length sample) / (2 :: Double)) sample

iqr :: Sample -> Double
iqr sample = upperQuartile sample - lowerQuartile sample

lowerResiduals :: Sample -> [Double]
lowerResiduals sample =
  let q1 = lowerQuartile sample
      lowerLimit = q1 - 1.5 * iqr sample
  in filter (< lowerLimit) (getSample sample)

upperResiduals :: Sample -> [Double]
upperResiduals sample =
  let q3 = upperQuartile sample
      upperLimit = q3 + 1.5 * iqr sample
  in filter (> upperLimit) (getSample sample)

residuals :: Sample -> [Double]
residuals sample = lowerResiduals sample ++ upperResiduals sample

isResidual :: Double -> Sample -> Bool
isResidual value sample = value `elem` residuals (insertSample value sample)
