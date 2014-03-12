module Stats where

import Prelude hiding (sum, length)

import qualified Data.Foldable as Foldable
import Data.Foldable (Foldable)

import qualified Data.Vector.Generic as Vector
import Data.Vector.Generic (Vector)

newtype Mean a = Mean a deriving (Show)
newtype MVUE a = MVUE a deriving (Show)
newtype StdDev a = StdDev a deriving (Show)

-- length of vector
length :: (Num a, Vector v a) => v a -> a
length = fromIntegral . Vector.length

-- sum of the numbers
sum :: (Num a, Foldable v) => v a -> a
sum = Foldable.sum

-- mean of the numbers
mean :: (Fractional a, Foldable v, Vector v a) => v a -> a
mean xs = sum xs / length xs

-- squared difference from n
sqrerr :: (Num a) => a -> a -> a
sqrerr n = (^ (2::Int)) . (subtract n)

-- minimum variance unbiased estimator (variance of the numbers)
mvue :: (Fractional a, Foldable v, Vector v a) => v a -> a
mvue xs = sum (Vector.map err xs) / (length xs - 1)
    where
        mu = mean xs
        err = (mu `sqrerr`)

stddev :: (Floating a, Foldable v, Vector v a) => v a -> a
stddev = sqrt . mvue

far :: (Num a, Ord a) => StdDev a -> Mean a -> a -> a -> Bool
far (StdDev sd) (Mean mu) n x = abs (mu - x) > (n * sd)

-- eof
