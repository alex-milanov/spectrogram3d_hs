import System.Environment (getProgName, getArgs)
import Control.Concurrent (forkFinally) -- , threadDelay)
import Text.Printf (printf)
import Data.IORef (IORef, newIORef, atomicModifyIORef', readIORef) -- , modifyIORef)
import Data.Monoid (mconcat)

import qualified Graphics.UI.GLUT as GLUT
import qualified Control.Exception as Except

import qualified Data.Vector as Vector
import Data.Vector (Vector)

import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (<|))

import qualified Audiomain
import Audiomain (AudioState, audio_main)

import qualified Data.Foldable as Foldable

import Prelude as P
import Data.Vec as Vec hiding (toList)
import Graphics.GPipe

import Lib.AnimUtils
import Lib.RenderState
import Lib.Instance
import Lib.TypeNames

import qualified Stats

axesStream :: PrimitiveStream Line (VertexPosition, VertexRGBA)
axesStream = let mk vs c = toGPUStream LineStrip $ zip (P.map homPoint vs) (repeat c)
    in mconcat [ mk [(-1):.0:.0:.(), 1:.0:.0:.(), 0.8:.(-0.1):.0:.(), 0.8:.0:.0:.(), 1:.(-0.1):.0:.()]       (1:.0:.0:.1:.())
               , mk [0:.(-1):.0:.(), 0:.1:.0:.(), 0:.0.8:.(-0.1):.(), 0:.0.9:.(-0.05):.(), 0:.1:.(-0.1):.()] (0:.1:.0:.1:.())
               , mk [0:.0:.(-1):.(), 0:.0:.1:.(), (-0.1):.0:.0.8:.(), (-0.1):.0:.1:.()]                      (0:.0:.1:.1:.())
               ]

type Stream = PrimitiveStream Line (Vertex Float, Vertex Float)
type DisplayState = (Float, Float, Seq Stream)

type Stream2 = PrimitiveStream Line (VertexPosition, VertexRGBA)

args :: IO (Int, Int)
args = do
    argv <- getArgs
    tup@(stride, offset) <- parse argv
    printf "Stride: %d\n" stride
    printf "Offset: %d\n" offset
    return tup
    where
        parse :: [String] -> IO (Int, Int)
        parse v@[s1, s2] = do
            printf "Reading two ints.. %s\n" (show v)
            return (read s1, read s2)
        parse _ = do
            printf "Using defaults..\n"
            return (512, 512)

main :: IO ()
main = do
    GLUT.getArgsAndInitialize
    (stride, offset) <- args
    n <- getProgName
    r <- newIORef (Audiomain.initial stride)

    -- audio thread
    forkFinally (audio_main r stride offset) $ \me -> case me of
        Left e -> printf "audio monitor thread crashed (%s)\n" (show e)
        Right _ -> printf "audio monitor thread finished\n"

    -- display thread
    r3 <- newIORef (fromIntegral stride, 1, Seq.empty)
    Except.catch (display_main n r r3 displayct) $ \e -> do
        printf "graphics mainloop crashed (%s)\n" (show (e::Except.SomeException))
    printf "graphics mainloop finished\n"
    where
        displayct :: Int
        displayct = 40 -- how many ffts to display on the screen

display_main :: String -> IORef AudioState -> IORef DisplayState -> Int -> IO ()
display_main n r r3 displayct = do
    newWindow n (vec 0) (768:.512:.()) (displayIO r r3 displayct) initWindow
    GLUT.mainLoop
    where
        initWindow :: GLUT.Window -> IO ()
        initWindow w = do
            GLUT.idleCallback GLUT.$= Just (GLUT.postRedisplay $ Just w)
            GLUT.keyboardMouseCallback GLUT.$= Just onKeyMouse
            --GLUT.depthClamp GLUT.$= GLUT.Enabled
        onKeyMouse :: GLUT.Key -> GLUT.KeyState -> GLUT.Modifiers -> GLUT.Position -> IO ()
        onKeyMouse (GLUT.Char '\ESC') GLUT.Down _ _ = GLUT.leaveMainLoop
        onKeyMouse _ _ _ _ = return ()


displayIO :: IORef AudioState -> IORef DisplayState -> Int -> Vec2 Int -> IO (FrameBuffer RGBAFormat DepthFormat ())
displayIO r r3 displayct size = do
    rs <- mkRenderState size
    au <- readIORef r
    (freq_max, amp_max, ds) <- atomicModifyIORef' r3 (maybe_displaylist au displayct)
    printf "Freqmax: %f, Ampmax: %f\n" freq_max amp_max
    let yscale = toGPU $ height / amp_max :: Vertex Float
    return $ display rs (Seq.mapWithIndex (stream_prep yscale displayct) ds)
    where
        height = 0.8

-- Conditionally update the display list when an fft has occurred.
maybe_displaylist :: AudioState -> Int ->DisplayState -> (DisplayState, DisplayState)
maybe_displaylist au displayct displaystate_ = (displaystate, displaystate)
    where
        displaystate = maybe displaystate_
                       (displaylist displayct displaystate_)
                       --(Audiomain.m_fft au)
                       (Just . Vector.fromList . Foldable.toList $ Audiomain.m_vs au)

-- Given an FFT, generate a new display list.
displaylist :: Int -> DisplayState -> Vector Float -> DisplayState
displaylist displayct (fmax_, amax_, streams_) disp_ = (fmax, amax, streams)
    where
        -- update the maximum interesting frequency
        -- limit display to the frequencies which are interesting
        fmax = learn_freqmax fmax_ disp_
        disp = Vector.take (round fmax) disp_
        -- maximum magnitude (amplitude) among frequencies (used to vertical-scale the display)
        --amax = mix smax_ (Vector.maximum disp) 0.005
        aavg = Vector.sum disp / fromIntegral (Vector.length disp)
        amax = let dispmax = if Vector.null disp then amax_ else Vector.maximum disp
               in mix (max amax_ dispmax) aavg 0.005
        -- wavg 0.999 (max max' $ P.abs v) avg'
        -- x values for the line segments in the stream
        xs = let len = Vector.length disp
                 x idx = mix (-0.5) (0.5) (fromIntegral idx / fromIntegral len)
             in P.map x ([0..] :: [Int])
        -- combine x values with y and make a gpu stream
        stream = toGPUStream LineStrip . zip xs . Foldable.toList $ disp
        -- attach this stream to the state
        streams = Seq.take displayct (stream <| streams_)

-- Shift, scale, and color a stream of (x, y) with its index, to a stream of (VertexInput, VertexRGBA)
stream_prep :: Vertex Float -> Int -> Int -> Stream -> Stream2
stream_prep yscale displayct idx strm = fmap f strm
    where
        old = toGPU $ fromIntegral idx / fromIntegral displayct :: Vertex Float
        -- SHADER
        f (x, y_) = let y = (sin (pi * old) * (-1) * 0.5) + (y_ * yscale)
                        z = mix 0.5 (-5) old
                        a = mix 1 0.01 old
                   in (homPoint $ x:.y:.z:.(), 0:.0:.0:.a:.())

learn_freqmax :: Float -> Vector Float -> Float
learn_freqmax fmax_ v = -- if Vector.null fariv
                        -- then fmax_
                        -- else mix fmax_ (fromIntegral $ Vector.last fariv) 0.05
                        fromIntegral $ Vector.length v
    where
        mu = Stats.mean v
        sd = Stats.stddev v
        far = Stats.far (Stats.StdDev sd) (Stats.Mean mu) 1
        fariv = Vector.findIndices (==True) $ Vector.map far v

display :: RenderState Float -> Seq Stream2 -> FrameBuffer RGBAFormat DepthFormat ()
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
                in Instance s r t Nothing . mconcat . Foldable.toList $ wavs
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
