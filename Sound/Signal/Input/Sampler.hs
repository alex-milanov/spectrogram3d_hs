module Sound.Signal.Input.Sampler
( weightedAverage
, Sampler(frame)
, Full(), Skip(), ModMean(), RunMean(), Biggest(), Smallest(), Mag()
, full,   skip,   modMean,   runMean,   biggest,   smallest,   mag
) where

-- Recommended import
-- import qualified Sound.Signal.Input.Sampler as Sampler
-- import Sound.Signal.Input.Sampler (Sampler)

import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (|>))

import qualified Data.Foldable as Foldable
import qualified Sound.Signal.Util as Util
-- import qualified Data.Maybe as Maybe

-- Weighted average with weight on middle argument.
weightedAverage :: Num n => n -> n -> n -> n
weightedAverage w a b = a*w + b*(1-w)

-- A sampler accumulates frames before issuing a new sample.
class Sampler s where
    accum :: (Ord a, Fractional a) => a -> s a -> s a
    probe :: (Ord a, Fractional a) => s a -> Maybe a
    frame :: (Ord a, Fractional a) => a -> s a -> (s a, Maybe a)
    frame v samp_ = let samp = accum v samp_ in (samp, probe samp)

-- No downsampling. Return every frame.
newtype Full a = Full a
    deriving (Show)

full :: a -> Full a
full initial = Full initial

instance Sampler Full where
    accum v (Full _) = Full v
    probe (Full v) = return v

-- Simple downsampling. Return one value every 'size' frames.
newtype Skip a = Skip (Int, Int, a)
    deriving (Show)

skip :: Num a => Int -> Maybe (Skip a)
skip size = Util.when (size >= 2) $ Skip (size, 0, 0)

instance Sampler Skip where
    accum v (Skip (size, ct, _)) = Skip (size, (ct + 1) `mod` size, v)
    probe (Skip (_, ct, v)) = Util.when (ct == 0) v

-- Return the average of every 'size' frames.
newtype ModMean a = ModMean (Int, Int, a)
    deriving (Show)

modMean :: Num a => Int -> Maybe (ModMean a)
modMean size = Util.when (size >= 2) $ ModMean (size, 0, 0)

instance Sampler ModMean where
    accum v (ModMean (size, ct, tot)) = ModMean (size, (ct + 1) `mod` size, tot + v)
    probe (ModMean (size, ct, tot)) = Util.when (ct == 0) (tot / fromIntegral size)

-- Return a running average every frame with 'weight' on the previous average.
newtype RunMean a = RunMean (a, a)
    deriving (Show)

runMean :: (Num a, Ord a) => a -> a -> Maybe (RunMean a)
runMean weight initial = Util.when (0 < weight && weight < 1) $ RunMean (weight, initial)

instance Sampler RunMean where
    accum v (RunMean (weight, avg)) = RunMean (weight, weightedAverage weight avg v)
    probe (RunMean (_, avg)) = return avg

-- Return the maximum of the last 'size' frames.
newtype Biggest a = Biggest (Int, Seq a)
    deriving (Show)

biggest :: Int -> Maybe (Biggest a)
biggest size = Util.when (size >= 2) $ Biggest (size, Seq.empty)

instance Sampler Biggest where
    accum v (Biggest (size, window)) = Biggest (size, Seq.take size $ window |> v)
    probe (Biggest (_, window)) = return $ Foldable.foldl1 max window

-- Return the minimum of the last 'size' frames.
newtype Smallest a = Smallest (Int, Seq a)
    deriving (Show)

smallest :: Int -> Maybe (Smallest a)
smallest size = Util.when (size >= 2) $ Smallest (size, Seq.empty)

instance Sampler Smallest where
    accum v (Smallest (size, window)) = Smallest (size, Seq.take size $ window |> v)
    probe (Smallest (_, window)) = return $ Foldable.foldl1 min window

-- The same behavior as the wrapped sampler if all input passed through 'abs'.
newtype Mag s a = Mag (s a)
    deriving (Show)

mag :: Sampler s => s a -> Mag s a
mag sampler = Mag sampler

instance Sampler s => Sampler (Mag s) where
    accum v (Mag sampler) = Mag $ accum (abs v) sampler
    probe (Mag sampler) = probe sampler

-- -- The same behavior as the wrapped sampler, but always probes a sample paired with "freshness".
-- newtype Cached s a = Cached (s a, a)
--     deriving (Show)
-- 
-- cached :: Sampler s => s a -> a -> Cached s a
-- cached sampler initial = Cached (sampler, initial)
-- 
-- instance Sampler s => Sampler (Cached s) where
--     accum v (Cached (sampler_, prev)) = let sampler = accum v sampler_
--                                         in Cached (sampler, maybe prev id $ probe sampler)
--     probe (Cached (sampler, prev)) = return (prev, Maybe.isJust $ probe sampler)

-- -- The same behavior as the wrapped samplers, but probes a value whenever either sampler probes a value.
-- -- When one sampler probes a value and the other doesn't, will present a cached sample from the one which didn't.
-- newtype Union s s2 a a2 = Union (s a, a, s2 a2, a2)
--     deriving (Show)
-- 
-- instance (Sampler s, Sampler s2) => Sampler (Union s s2) where
--     accum v (Union (samp, prev, samp2, prev2)) =
--         Union ( accum v samp, maybe prev id (probe samp)
--               , accum v samp2, maybe prev2 id (probe samp2)
--               )
--     probe (Union (samp, prev, samp2, prev2)) = 

-- -- The same behavior as the wrapped samplers, but combined with a function.
-- -- f :: a -> a2 -> a3
-- newtype Zip s s2 a a2 f a3 = Zip (s a, s2 a2, f, a3)
--     deriving (Show)
-- 
-- instance (Sampler s, Sampler s2) => Sampler (Zip s s2) where
--     accum v (Zip (samp, samp2, comb, prev)) = Zip ( accum 

-- eof
