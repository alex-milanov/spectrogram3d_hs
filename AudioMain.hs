{-# LANGUAGE RecordWildCards #-}
module AudioMain where

-- Recommended import
--import qualified AudioMain
--import AudioMain (AudioState, audio_main)

-- qualified

import qualified GHC.Float
import qualified Data.Foldable as Foldable
import qualified Foreign.C.Types as CTypes
import qualified Sound.Signal.FFT as FFT
import qualified Sound.JACK.Audio as JACK
import qualified Data.Vector.Generic.Stats as Stats

-- mixed

import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (<|))

import qualified Data.Vector as Vector
import Data.Vector (Vector)

import qualified Data.IORef as IORef
import Data.IORef (IORef)

-- unqualified

import Text.Printf (printf)

type JackFrame = (JACK.Sample, JACK.Sample)
type Frame = Float

data AudioState = Mono
                  { m_vs :: !(Seq Float)
                  , m_ampavg :: !Float
                  , m_fftct :: !Int
                  , m_fft :: !(Maybe (Vector Float))
                  }
    deriving (Show)

initial :: Int -> AudioState
initial stride = Mono { m_vs = Seq.replicate stride 0
                      , m_ampavg = 0
                      , m_fftct = 0
                      , m_fft = Nothing
                      }

audio_main :: IORef AudioState -> Int -> Int -> IO ()
audio_main audioref stride offset = do
    printf "Planning..\n"
    p <- FFT.plan stride
    printf "done.\n"
    msgref <- IORef.newIORef (0, "")
    JACK.mainStereo $ \fr -> do
        -- deal with the frame, maybe returning an error message
        ms <- IORef.atomicModifyIORef' audioref (frame stride offset p $ mono fr)
        -- produce a human readable error; suppress repeated messages
        let msg = maybe "" (printf "audio_main %s: %s" (show fr)) ms :: String
        printer <- IORef.atomicModifyIORef' msgref (msgcombine msg)
        printer
        return (0, 0) 

msgcombine :: String -> (Int, String) -> ((Int, String), IO ())
msgcombine cur (ct_, prev)
    | cur == ""   = ((ct_, prev), return ())
    | cur == prev = ((ct, prev), printf "\r%dx  " ct)
    | otherwise   = ((0, cur), printf "%s\n" cur)
    where
        ct = ct_ + 1

mono :: JackFrame -> Frame
mono (CTypes.CFloat left, CTypes.CFloat right) = (left + right) / 2

-- update the state, sometimes generating an fft
frame :: Int -> Int -> FFT.Plan -> Frame -> AudioState -> (AudioState, Maybe String)
frame stride offset p fr au_ | fftct == 0 = step_fft p au
                             | otherwise = (au, Nothing)
    where
        au@Mono { m_fftct = fftct } = step_always stride offset fr au_

-- update parts of the state which must update for every audio frame
step_always :: Int -> Int -> Frame -> AudioState -> AudioState
step_always stride offset fr (au@Mono {..}) = au { m_vs = Seq.take stride $ fr <| m_vs
                                                 , m_fftct = (m_fftct + 1) `mod` offset
                                                 }

-- update parts of the state which only update when it's time to make an fft
step_fft :: FFT.Plan -> AudioState -> (AudioState, Maybe String)
step_fft p (au@Mono {..}) = ( au { m_ampavg = ampavg
                                 , m_fft = fft
                                 }
                            , msg
                            )
    where
        wave = Vector.fromList . Foldable.toList $ m_vs
        -- learn the amplitude of the raw audio
        ampavg = maybe (error $ printf "learn_ampavg %f $ wave=%s" m_ampavg $ show wave)
                       id
                       (learn_ampavg m_ampavg $ Vector.map abs wave)
        -- scale audio average to 0.5
        -- FIXME: should be in an AudioConf object
        -- FIXME: - completely breaks QUIET sections in the spectrogram
        --        - it magnifies white noise in silence
        scale = 1 -- 0.5 / ampavg
        scalef = (* scale)
        -- window (makes the beginning and end of this chunk quieter for better fft
        windowf idx samp = samp * hamming (fromIntegral idx) len
            where
                len = fromIntegral $ Vector.length wave
        -- generate an fft
        (fft, msg) = either (\s -> (m_fft, Just s))
                            (\v -> (Just v, Nothing))
                            (mk_fft p $ Vector.imap windowf $ Vector.map scalef wave)

hamming :: Floating a => a -> a -> a
hamming n len = a - b * (cos $ 2 * pi * rat)
    where
        rat = n / len
        a = 0.53836
        b = 0.46164

-- learn the maximum of the vector; decay toward the average
learn_ampavg :: Float -> Vector Float -> Maybe Float
learn_ampavg aavg_ v = do
    vmu <- Stats.mean v
    return $ wavg aavg_ vmu 0.001

mk_fft :: FFT.Plan -> Vector Float -> Either String (Vector Float)
mk_fft p amplitude_fv = do
    frequency_dv <- FFT.rfftdb (Just 1) p . fmap GHC.Float.float2Double $ amplitude_fv
    let ct = Vector.length frequency_dv `div` 2
    return . fmap GHC.Float.double2Float . Vector.take ct $ frequency_dv

wavg :: Num n => n -> n -> n -> n
wavg a b w = a*w + b*(1-w)

-- eof
