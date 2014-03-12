module Sound.Signal.FFT
( Plan()
, plan
, fft
, minclip
, rfftdb
) where

import qualified Data.Vector as Vector
import Data.Vector (Vector)

import qualified Data.Complex as Complex
import Data.Complex (Complex(..))

import qualified Numeric.FFT as FFT

-- Store plan size with plan
data Plan = Plan Int FFT.Plan
    deriving (Show)

-- Smart constructor makes plan and stores size with it
plan :: Int -> IO Plan
plan size = do
    p <- FFT.plan size
    return (Plan size p)

-- Do an FFT if vector has same size as plan
fft :: Plan -> Vector (Complex Double) -> Either String (Vector (Complex Double))
fft (Plan size p) v
    | Vector.length v == size = Right $ FFT.fftWith p v
    | otherwise = Left "Plan size doesn't match input vector size"

-- Replace everything <clip with the smallest input value >=clip OR fill value if given
minclip :: (Ord a) => a -> Maybe a -> Vector a -> Either String (Vector a)
minclip clip mfill v
    | Vector.null ge_clip_v = Left "No values were greater than or equal to the clip value"
    | otherwise = Right $ fmap (\n -> if n < clip then fill else n) v
    where
        ge_clip_v = Vector.filter (>= clip) v
        ge_clip = Vector.minimum ge_clip_v
        fill = maybe ge_clip id mfill
-- let eg = fromList [-0.5, 0.5, 1.5, 2.5, 3.5]
-- minclip  1 Nothing  eg == Right (fromList [1.5, 1.5, 1.5, 2.5, 3.5])
-- minclip  1 (Just 2) eg == Right (fromList [2.0, 2.0, 1.5, 2.5, 3.5])
-- minclip 10 Nothing  eg == Left
-- minclip 10 (Just 2) eg == Left

-- Do an fft on reals and give back the magnitudes of their complex result
-- Magnitude values in the result <1 are set to the minimum >=1 in the result (or fill, if given)
-- Result is then scaled to dB
rfftdb :: Maybe Double -> Plan -> Vector Double -> Either String (Vector Double)
rfftdb fill p av = do
    raw_fv <- fft p . fmap (:+ 0) $ av
    clipped_fv <- minclip 1 fill . fmap Complex.magnitude $ raw_fv
    return $ fmap ((10 *) . logBase 10) clipped_fv

-- eof
