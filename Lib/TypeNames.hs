module Lib.TypeNames
( CPUMatrix
, VertexMatrix
, VertexPosition
, VertexRGB
, VertexRGBA
) where

import Graphics.GPipe (Vertex, VertexPosition)
import Data.Vec

type CPUMatrix = Mat44 Float
type VertexMatrix = Mat44 (Vertex Float)

-- VertexPosition
type VertexRGB = Vec3 (Vertex Float)
type VertexRGBA = Vec4 (Vertex Float)
