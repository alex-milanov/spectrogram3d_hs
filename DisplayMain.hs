{-# LANGUAGE RecordWildCards #-}
module DisplayMain where

-- Recommended import
--import qualified DisplayMain
--import DisplayMain (DisplayConf, DisplayState, display_main)

-- qualified 

import qualified Control.Monad as Monad
import qualified Data.Monoid as Monoid
import qualified Data.Foldable as Foldable

-- mixed

import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (<|))

import qualified Data.IORef as IORef
import Data.IORef (IORef)

import qualified Data.Vector as Vector
import Data.Vector (Vector)

import qualified Graphics.UI.GLUT as GLUT
import Graphics.UI.GLUT (($=))

import qualified Data.Vector.Generic.Stats as Stats
import Data.Vector.Generic.Stats (Mean(..), StdDev(..)) --, MVUE(..))

import qualified AudioMain
import AudioMain (AudioState)

-- unqualified

import Prelude as P
import Data.Vec as Vec hiding (toList)
import Graphics.GPipe

import Lib.AnimUtils
import Lib.Instance
import Lib.TypeNames
import Lib.RenderState

import Text.Printf (printf)
--import Graphics.GLTut.RenderState (mkRenderState, RenderState)

data DisplayState = Dynamic
                    { ds_freqmax :: Float
                    , ds_ampmax :: Float
                    , ds_ffts :: Seq StateStream
                    }
--  deriving (Show)

type StateStream = PrimitiveStream Line (Vertex Float, Vertex Float)
type DisplayStream = PrimitiveStream Line (VertexPosition, VertexRGBA)

type Frame = FrameBuffer RGBAFormat DepthFormat ()

data DisplayConf = Static
                   { dc_name :: String
                   , dc_linect :: Int
                   , dc_lineheight :: Float
                   }
    deriving (Show)

initial :: String -> Int -> Float -> (DisplayState, DisplayConf)
initial name linect lineheight =
    ( Dynamic
      { ds_freqmax = 0
      , ds_ampmax = 0
      , ds_ffts = Seq.empty
      }
    , Static
      { dc_name = name
      , dc_linect = linect
      , dc_lineheight = lineheight
      }
    )

display_main :: DisplayConf -> IORef DisplayState -> IORef AudioState -> IO ()
display_main dispconf rdispst raudst = do
    GLUT.getArgsAndInitialize
    newWindow (dc_name dispconf)
              (vec 0)
              (768:.512:.())
              (displayIO dispconf rdispst raudst)
              $ \w -> do
        GLUT.idleCallback $= Just (GLUT.postRedisplay $ Just w)
        GLUT.keyboardMouseCallback $= Just onKeyMouse
    GLUT.mainLoop

onKeyMouse :: GLUT.KeyboardMouseCallback
onKeyMouse (GLUT.Char '\ESC') GLUT.Down _ _ = GLUT.leaveMainLoop
onKeyMouse _                  _         _ _ = return ()

displayIO :: DisplayConf -> IORef DisplayState -> IORef AudioState -> Vec2 Int -> IO Frame
displayIO dispconf rdispst raudst size = do
    rendst <- mkRenderState size
    -- feed the fft into the display state
    AudioMain.Mono {m_fft=fft, m_ampavg=m_ampavg, m_fftct=m_fftct} <- IORef.atomicModifyIORef' raudst
        $ \audst -> (audst {AudioMain.m_fft=Nothing}, audst)
    dispst@Dynamic {..} <- IORef.atomicModifyIORef' rdispst (split $ consume_fft dispconf fft)
--  printf "ds_ampmax: %3.3f, ds_freqmax: %3.2f, m_ampavg: %.6f, m_fftct: %d\n" ds_ampmax ds_freqmax m_ampavg m_fftct
    -- update screen
    return . display rendst . prep_displaylist dispconf $ dispst

-- transform a function such that it returns its output twice
split :: (a -> b) -> (a -> (b, b))
split f x = let r = f x in (r, r)

-- fold a new fft state into the display state
consume_fft :: DisplayConf -> Maybe (Vector Float) -> DisplayState -> DisplayState
consume_fft _             Nothing     dispst_        = dispst_
consume_fft (Static {..}) (Just fft__) (Dynamic {..}) =
    Dynamic { ds_freqmax = freqmax
            , ds_ampmax = ampmax
            , ds_ffts = streams
            }
    where
        -- increase spectrogram contrast
        fft_ = fmap (^(3::Int)) fft__
        -- update the maximum interesting frequency
        freqmax = maybe (error $ printf "learn_freqmax %f $ fft_=%s" ds_freqmax $ show fft_)
                        id
                        (learn_freqmax ds_freqmax fft_)
        -- limit display to the frequencies which are interesting
        fft = Vector.take (round freqmax) $ fft_
        -- update the maximum amplitude among frequencies to scale the display vertically
        ampmax = maybe (error $ printf "learn_ampmax %f $ fft=%s" ds_ampmax $ show fft)
                       id
                       (learn_ampmax ds_ampmax fft) -- FIXME: should this be abs?
        -- x values for the line segments in the stream
        xs = let len = Vector.length fft
                 x idx = mix (-0.5) (0.5) (fromIntegral idx / fromIntegral len) -- FIXME: can len be zero?
             in P.map x ([0..] :: [Int])
        -- combine x values with y and make a gpu stream
        stream = toGPUStream LineStrip . zip xs . Foldable.toList $ fft
        -- attach this stream to the state
        streams = Seq.take dc_linect (stream <| ds_ffts)

-- learn the maximum of the vector; decay toward the average
learn_ampmax :: Float -> Vector Float -> Maybe Float
learn_ampmax amax_ v = do
    vmu <- Stats.mean v
    vmax <- Stats.maximum v
    return $ mix (max amax_ vmax) vmu 0.001

-- learn the maximum index which is far from the stddev of the vector; decay toward zeno-zero
learn_freqmax :: Float -> Vector Float -> Maybe Float
learn_freqmax fmax_ v = do -- fromIntegral $ Vector.length v
    mu <- Stats.mean v
    sd <- Stats.stddev_ (Just$Left$Mean mu) v
    let far = Stats.far (StdDev sd) (Mean mu)
        fariv = Vector.findIndices (far 1) v
    Monad.guard (not $ Vector.null fariv)
    let farivmax = fromIntegral $ Vector.last fariv
    return $ mix (max fmax_ farivmax) (fmax_ / 2) 0.0065
    -- mix fmax_ (fromIntegral $ Vector.last fariv) 0.05 -- this one has a saner decay, but grows too slowly

-- prep each of the fft lines in the display state
prep_displaylist :: DisplayConf -> DisplayState -> Seq DisplayStream
prep_displaylist (Static {..}) (Dynamic {..}) = Seq.mapWithIndex prep_fft' ds_ffts
    where
        ampmax = toGPU $ ds_ampmax
        -- FIXME: 1. ampmax here in the yscale calculation is kind of unnecessary
        --           if audio is normalized before fft because ffts will all have same height ..
        --               eg. i can replace it right now with 22 and the height in the visualizer is fine
        --        2. it's still important for the coloring function though
        yscale = toGPU $ dc_lineheight / ds_ampmax
        prep_fft' = prep_fft ampmax yscale dc_linect

-- prep one fft line for display
prep_fft :: Vertex Float -> Vertex Float -> Int -> (Int -> StateStream -> DisplayStream)
prep_fft ampmax yscale linect index stream = fmap prep stream
    where
        distance = toGPU $ fromIntegral index / fromIntegral linect
        -- vertex shader; shift, scale, and color a primitivestream
        prep :: (Vertex Float, Vertex Float) -> (VertexPosition, VertexRGBA)
        prep (x, y_) = let swoop = 0.5 * (-1) * sin (pi * distance)
                           y = swoop + (y_ * yscale)
                           z = mix 0.5 (-5) distance
                           r = mix 0 1 (y_ / ampmax)
                           a = mix 1 0.01 distance
                       in (homPoint $ x:.y:.z:.(), r:.0:.0:.a:.())

axesStream :: PrimitiveStream Line (VertexPosition, VertexRGBA)
axesStream = let mk vs c = toGPUStream LineStrip $ zip (P.map homPoint vs) (repeat c)
    in Monoid.mconcat [ mk [(-1):.0:.0:.(), 1:.0:.0:.(), 0.8:.(-0.1):.0:.(), 0.8:.0:.0:.(), 1:.(-0.1):.0:.()]       (1:.0:.0:.1:.())
                      , mk [0:.(-1):.0:.(), 0:.1:.0:.(), 0:.0.8:.(-0.1):.(), 0:.0.9:.(-0.05):.(), 0:.1:.(-0.1):.()] (0:.1:.0:.1:.())
                      , mk [0:.0:.(-1):.(), 0:.0:.1:.(), (-0.1):.0:.0.8:.(), (-0.1):.0:.1:.()]                      (0:.0:.1:.1:.())
                      ]

display :: RenderState Float -> Seq DisplayStream -> Frame
display rs wavs = P.foldl (flip draw) cleared
           $ let f = P.map (mkFrags world2clip)
             in f trs ++ f lns ++ f pts
    where
        sec = rsSeconds rs
        blend = Blend (FuncAdd, FuncAdd)
                      ((SrcAlpha, OneMinusSrcAlpha), (One, Zero))
                      (RGBA (vec 1) 1)
        draw = paintColorRastDepth Less True blend (RGBA (vec True) True)
        cleared = newFrameBufferColorDepth (RGBA (vec 1) 1) 1

        cam = let a = easeMiddUpDownUp sec 16 `onRange` (1, -1)
                  b = easeThereAndBack sec 16 `onRange` (1, 1.75)
              in a:.0.6:.b:.()

        cam2clip = let zNear = 0.01
                       zFar = 1000
                       fovDeg = 45
                   in perspective zNear zFar (fovDeg * pi / 180) (rsAspectRatio rs)
        world2cam = let up = 0:.1:.0:.()
                        tgt = (easeMiddUpDownUp sec 16 `onRange` (0.2, -0.2)):.0.2:.0:.()
                    in multmm (transpose $ rotationLookAt up cam tgt) (translation $ -cam)
        world2clip = multmm cam2clip world2cam 

        trs = []
        lns = [ Instance (vec 1) [] (vec 0) Nothing axesStream
              , Instance (vec 0.1) [] (0:.0.1:.0.5:.()) Nothing axesStream
              , let s = 2:.0.5:.1:.()
                    r = []
                    t = 0:.(-0.1):.(-0.05):.()
                in Instance s r t Nothing . Monoid.mconcat . Foldable.toList $ wavs
              ]
        pts = []

mkFrags :: CPUMatrix -> Instance Float p (VertexPosition, VertexRGBA)
        -> FragmentStream (Color RGBAFormat (Fragment Float)) 
mkFrags world2clip i = fmap (\rgba -> RGBA (Vec.take n3 rgba) (Vec.last rgba))
                     $ rasterizeFront
                     $ fmap (vShdr $ toGPU model2clip)
                     $ instStream i
    where
        model2world = instWorldMatrix i
        model2clip = multmm world2clip model2world

vShdr :: VertexMatrix -> (VertexPosition, VertexRGBA) -> (VertexPosition, VertexRGBA)
vShdr model2clip (p, c) = (multmv model2clip p, c)

-- eof
