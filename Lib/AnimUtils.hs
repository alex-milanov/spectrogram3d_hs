module Lib.AnimUtils where

import Data.Fixed (mod')

-- Compute a real [0, 1) which ranges linearly on 0..1 as elapsedTime ranges on 0..loopDuration
lerpThere :: Float -> Float -> Float
lerpThere elapsedTime loopDuration = mod' elapsedTime loopDuration / loopDuration

-- Compute a real [0, 1] which ranges linearly on 0..1..0 as elapsedTime ranges on 0..loopDuration
lerpThereAndBack :: Float -> Float -> Float
lerpThereAndBack elapsedTime loopDuration = 2 * if f > 0.5 then 1 - f else f
    where f = lerpThere elapsedTime loopDuration

-- Compute a real [0, 1) which eases heavily on 0..1 as elapsedTime ranges on 0..loopDuration
easeThere :: Float -> Float -> Float
easeThere elapsedTime loopDuration = (negate . cos . (pi*) $ f) / 2 + 0.5
    where f = lerpThere elapsedTime loopDuration

-- Compute a real [0, 1] which eases heavily on 0..1..0 as elapsedTime ranges on 0..loopDuration
easeThereAndBack :: Float -> Float -> Float
easeThereAndBack elapsedTime loopDuration = (negate . cos . (pi*2*) $ f) / 2 + 0.5
    where f = lerpThere elapsedTime loopDuration

-- Compute a real [0, 1] which eases heavily on 0.5..1..0..0.5 as elapsedTime ranges on 0..loopDuration
easeMiddUpDownUp :: Float -> Float -> Float
easeMiddUpDownUp elapsedTime loopDuration = (sin . (pi*2*) $ f) / 2 + 0.5
    where f = lerpThere elapsedTime loopDuration

-- Shift and scale a fraction [0, 1] onto the given range.
onRange :: Float -> (Float, Float) -> Float
v `onRange` (mn, mx) = v * r + mn
    where r = mx - mn

-- eof
