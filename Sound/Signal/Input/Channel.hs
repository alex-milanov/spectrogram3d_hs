module Sound.Signal.Input.Channel
( Channel()
, channel, metric
, frame
) where

-- Recommended import
-- import qualified Sound.Signal.Input.Channel as Channel
-- import Sound.Signal.Input.Channel (Channel)

import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (|>))

import qualified Sound.Signal.Input.Sampler as Sampler
import Sound.Signal.Input.Sampler (Sampler)



-- A channel maintains a window of samples.
data Channel s a = Channel Int (s a) (Seq a)
    deriving (Show)

channel :: Int -> s a -> Channel s a
channel size sampler = Channel size sampler Seq.empty

frame :: (Sampler s, Fractional a, Ord a) => a -> Channel s a -> Channel s a
frame fr (Channel size sampler_ window) = Channel size sampler (frameWindow size mfr window)
    where
        (sampler, mfr) = Sampler.frame fr sampler_

-- probe

accumWindow :: Maybe a -> Seq a -> Seq a
accumWindow mfr window = maybe window (window |>) mfr

refocusWindow :: Int -> Seq a -> Seq a
refocusWindow size window = Seq.drop (Seq.length window - size) window

frameWindow :: Int -> Maybe a -> Seq a -> Seq a
frameWindow size mfr = refocusWindow size . accumWindow mfr

-- A metric maintains one sample.
metric :: s a -> Channel s a
metric sampler = channel 1 sampler

-- A meanChannel 

-- eof
