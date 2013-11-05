module Lib.Instance where

import Prelude as P
import Data.Vec
import Graphics.GPipe

data Axis = X | Y | Z

axisRotFun :: (Floating a) => Axis -> a -> Mat44 a
axisRotFun X = rotationX
axisRotFun Y = rotationY
axisRotFun Z = rotationZ

data Instance a p d = Instance { instScale :: Vec3 a
                               , instRotation :: [(Axis, a)]
                               , instTranslation :: Vec3 a
                               , instOverrideMatrix :: Maybe (Mat44 a)
                               , instStream :: PrimitiveStream p d
                               }


instWorldMatrix :: (Floating a) => Instance a p d -> Mat44 a
instWorldMatrix Instance { instOverrideMatrix = Just m } = m
instWorldMatrix i = foldl1 multmm [ translation $ instTranslation i
                                  , rotm
                                  , scaling $ instScale i ]
    where
        rotm = P.foldl multmm identity
             $ P.map (\(a, r)->axisRotFun a r) (instRotation i)

-- eof
