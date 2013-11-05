import System.Environment (getProgName)
import Control.Concurrent (forkFinally, threadDelay)
import Text.Printf (printf)
import Data.IORef (IORef, newIORef, atomicModifyIORef', readIORef)
import Sound.JACK.Audio (mainStereo, Sample)
import Data.Monoid (mconcat)

import qualified Foreign.C.Types as CT
import qualified Graphics.UI.GLUT as GLUT

import Prelude as P
import Data.Vec
import Graphics.GPipe

import Lib.AnimUtils
import Lib.RenderState
import Lib.Instance
import Lib.TypeNames

axesStream :: PrimitiveStream Line (VertexPosition, VertexRGB)
axesStream = let mk vs c = toGPUStream LineStrip $ zip (P.map homPoint vs) (repeat c)
    in mconcat [ mk [(-1):.0:.0:.(), 1:.0:.0:.(), 0.8:.(-0.1):.0:.(), 0.8:.0:.0:.(), 1:.(-0.1):.0:.()]       (1:.0:.0:.())
               , mk [0:.(-1):.0:.(), 0:.1:.0:.(), 0:.0.8:.(-0.1):.(), 0:.0.9:.(-0.05):.(), 0:.1:.(-0.1):.()] (0:.1:.0:.())
               , mk [0:.0:.(-1):.(), 0:.0:.1:.(), (-0.1):.0:.0.8:.(), (-0.1):.0:.1:.()]                      (0:.0:.1:.())
               ]

data AudioState = Mono { m_cc :: [Float]
                       , m_vs :: [Float]
                       , m_max :: Float
                       , m_avg :: Float
                       }
wavg w a b = a*w + b*(1-w)

main :: IO ()
main = do
    GLUT.getArgsAndInitialize
    n <- getProgName
    r <- newIORef $ Mono [] [] 0 0
    forkFinally (audio_main n r) $ \_ -> do
        printf "%s: audio monitor thread quit\n" n
    --threadDelay $ round 1e6
    display_main n r
    printf "%s: graphics mainloop returned\n" n

audio_main :: String -> IORef AudioState -> IO ()
audio_main n r = do
    mainStereo $ \fr -> do
        atomicModifyIORef' r $ \s -> (consumeFrame fr s, ())
        return fr
    where
        span = div 44100 4
        samples = 441
        chunk = div span samples
        -- consume the frame and update state
        consumeFrame :: (Sample, Sample) -> AudioState -> AudioState
        consumeFrame (CT.CFloat l, CT.CFloat r) d = update d'
            where
                mono = (l + r) / 2 
                d' = d { m_cc = mono : (m_cc d) }
        -- build the updated result tuple based on whether the current chunk is complete
        update :: AudioState -> AudioState
        update d
            | ct >= chunk = let v = P.head cc
                                --v = P.sum cc / (fromIntegral $ P.length cc)
                                --v = foldl1 P.max $ P.map P.abs cc
                                vs = P.take samples $ v : (m_vs d)
                                max' = m_max d
                                avg' = m_avg d
                            in d { m_cc = []
                                 , m_vs = vs
                                 , m_max = wavg 0.9999 (max max' v) avg'
                                 , m_avg = wavg 0.99 avg' (P.abs v)
                                 }
            | otherwise = d
            where
                cc = m_cc d
                ct = P.length cc

display_main :: String -> IORef AudioState -> IO ()
display_main n r = do
    newWindow n (vec 0) (768:.512:.()) (displayIO r) initWindow
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

displayIO :: IORef AudioState -> Vec2 Int -> IO (FrameBuffer RGBFormat DepthFormat ())
displayIO r size = do
    rs <- mkRenderState size
    d <- readIORef r
    let ct = fromIntegral $ P.length (m_vs d)
        xs = [0.5 - x | x <- [0,1 / (ct-1)..1]]
    let gmax = toGPU $ m_max d
        gavg = toGPU $ m_avg d
    --printf "overall max %.2f | running avg %.2f\n" (m_max d) (m_avg d)
    let stream = toGPUStream LineStrip $ zip xs (m_vs d) -- too big or too small!
        normmax = fmap (\(x, y) -> (x, y/gmax/2)) stream
        normavg = fmap (\(x, y) -> (x, y/gavg/2)) stream
    threadDelay $ round 1e4
    return $ display rs normmax

display :: RenderState Float -> PrimitiveStream Line (Vertex Float, Vertex Float) -> FrameBuffer RGBFormat DepthFormat ()
display rs wav = P.foldl (flip draw) cleared
           $ let f = P.map (mkFrags world2clip)
             in f trs ++ f lns ++ f pts
    where
        sec = rsSeconds rs
        draw = paintColorRastDepth Less True NoBlending (RGB $ vec True)
        cleared = newFrameBufferColorDepth (RGB $ vec 1) 1

        cam = let a = easeMiddUpDownUp sec 16 `onRange` (1, -1)
                  b = easeThereAndBack sec 16 `onRange` (1, 1.75)
              in a:.0.4:.b:.()

        cam2clip = let zNear = 0.01
                       zFar = 1000
                       fovDeg = 45
                   in perspective zNear zFar (fovDeg * pi / 180) (rsAspectRatio rs)
        world2cam = let up = 0:.1:.0:.()
                        tgt = (easeMiddUpDownUp sec 16 `onRange` (0.2, -0.2)):.0.05:.0:.()
                    in multmm (transpose $ rotationLookAt up cam tgt) (translation $ -cam)
        world2clip = multmm cam2clip world2cam 

        trs = []
        lns = [ Instance (vec 1) [] (vec 0) Nothing axesStream
              , let s = 2:.0.5:.1:.()
                    r = []
                    t = 0:.(0.1):.(-0.05):.()
                in Instance s r t Nothing $ fmap (\(x,y) -> (homPoint $ x:.y:.0:.(), vec 0)) wav
              ]
        pts = []

mkFrags :: CPUMatrix -> Instance Float p (VertexPosition, VertexRGB)
        -> FragmentStream (Color RGBFormat (Fragment Float)) 
mkFrags world2clip i = fmap RGB
                     $ rasterizeFront
                     $ fmap (vs $ toGPU model2clip)
                     $ instStream i
    where
        model2world = instWorldMatrix i
        model2clip = multmm world2clip model2world

vs :: VertexMatrix -> (VertexPosition, VertexRGB) -> (VertexPosition, VertexRGB)
vs model2clip (p, c) = (multmv model2clip p, c)

-- eof
