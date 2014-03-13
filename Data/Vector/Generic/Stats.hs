-- Statistics about Vectors which use Maybe to avoid NaN and _|_ results.
module Data.Vector.Generic.Stats where

-- Recommended import
--import qualified Data.Vector.Generic.Stats as Stats
--import Data.Vector.Generic.Stats (Mean(..), MVUE(..), StdDev(..))

import qualified Control.Monad as Monad

import qualified Data.Foldable as Foldable
import Data.Foldable (Foldable)

import qualified Data.Vector.Generic as Vector
import Data.Vector.Generic (Vector)

newtype Mean a = Mean a deriving (Show)
newtype MVUE a = MVUE a deriving (Show)
newtype StdDev a = StdDev a deriving (Show)

-- implementation of maximum which is never _|_
maximum :: (Vector v a, Ord a) => v a -> Maybe a
maximum v = do Monad.guard (not $ Vector.null v)
               return $ Vector.maximum v

-- implementation of minimum which is never _|_
minimum :: (Vector v a, Ord a) => v a -> Maybe a
minimum v = do Monad.guard (not $ Vector.null v)
               return $ Vector.minimum v

-- mean of numbers which is never NaN
mean :: (Fractional a, Foldable v, Vector v a) => v a -> Maybe a
mean xs = do Monad.guard (not $ Vector.null xs)
             return $ Foldable.sum xs
                    / (fromIntegral . Vector.length) xs

-- squared difference from n
sqrerr :: (Num a) => a -> a -> a
sqrerr n = (^ (2::Int)) . (subtract n)

--- --- ---
-- minimum variance unbiased estimator (variance of numbers)

-- mvue which doesn't calculate mean for you and is never NaN
mvue__ :: (Fractional a, Foldable v, Vector v a) => Mean a -> v a -> Maybe a
mvue__ (Mean mu) xs = do Monad.guard (Vector.length xs > 1)
                         return $ (Foldable.sum . Vector.map (mu `sqrerr`) $ xs)
                                / (fromIntegral $ Vector.length xs - 1)

-- mvue which optionally calculates mean for you
mvue_ :: (Fractional a, Foldable v, Vector v a) => Maybe (Mean a) -> v a -> Maybe a
mvue_ (Just mu) xs = mvue__ mu xs
mvue_ Nothing   xs = do mu <- mean xs
                        mvue__ (Mean mu) xs

-- mvue which always calculates mean for you
mvue :: (Fractional a, Foldable v, Vector v a) => v a -> Maybe a
mvue = mvue_ Nothing

--- --- ---
-- standard deviation

-- stddev which doesn't calculate mvue for you and is never NaN
stddev__ :: (Floating a, Ord a) => MVUE a -> Maybe a
stddev__ (MVUE var) = do
    Monad.guard (var >= 0)
    return $ sqrt var

-- stddev which optionally calculates mean xor mvue for you
stddev_ :: (Floating a, Ord a, Foldable v, Vector v a) => Maybe (Either (Mean a) (MVUE a)) -> v a -> Maybe a
stddev_ (Just (Right var)) _ = do stddev__ var
stddev_ (Just (Left mu))  xs = do var <- mvue_ (Just mu) xs
                                  stddev__ (MVUE var)
stddev_ Nothing           xs = do var <- mvue xs
                                  stddev__ (MVUE var)

-- -- stddev which always calculates mean and mvue for you
stddev :: (Floating a, Ord a, Foldable v, Vector v a) => v a -> Maybe a
stddev = stddev_ Nothing

-- is x more than n standard deviations away from the mean?
far :: (Num a, Ord a) => StdDev a -> Mean a -> a -> a -> Bool
far (StdDev sd) (Mean mu) n x = abs (mu - x) > (n * sd)

-- eof
