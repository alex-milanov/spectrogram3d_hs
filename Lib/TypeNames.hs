module Lib.TypeNames where

import Graphics.GPipe (Vertex, VertexPosition)
import Data.Vec

type CPUMatrix = Mat44 Float
type VertexMatrix = Mat44 (Vertex Float)

-- VertexPosition
type VertexRGB = Vec3 (Vertex Float)
