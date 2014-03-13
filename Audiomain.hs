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
                  , m_fftct :: !Int
                  , m_fft :: !(Maybe (Vector Float))
                  }
    deriving (Show)

initial :: Int -> AudioState
initial stride = Mono { m_vs = Seq.replicate stride 0
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

frame :: Int -> Int -> FFT.Plan -> Frame -> AudioState -> (AudioState, Maybe String)
frame stride offset p fr au = resolve offset p
                            . accum stride fr
                            $ au

accum :: Int -> Frame -> AudioState -> AudioState
accum stride fr au = au { m_vs = Seq.take stride $ fr <| m_vs au }

resolve :: Int -> FFT.Plan -> AudioState -> (AudioState, Maybe String)
resolve offset p (au@Mono {..}) = ( au { m_fftct = fftct
                                       , m_fft = fft
                                       }
                                  , msg
                                  )
    where
        fftct = (m_fftct + 1) `mod` offset
        (fft, msg) = maybe_fft p (Vector.fromList . Foldable.toList $ m_vs) fftct m_fft

-- When fftct is 0, calculate a new fft from the value-vector.
-- Otherwise, pass through the old fft.
maybe_fft :: FFT.Plan -> Vector Float -> Int -> Maybe (Vector Float) -> (Maybe (Vector Float), Maybe String)
maybe_fft p vv fftct fft_ | fftct == 0 = either (\msg -> (fft_, Just msg))
                                                (\fft -> (Just fft, Nothing))
                                                (mk_fft p vv)
                          | otherwise = (fft_, Nothing)

mk_fft :: FFT.Plan -> Vector Float -> Either String (Vector Float)
mk_fft p amplitude_fv = do
    frequency_dv <- FFT.rfftdb (Just 1) p . fmap GHC.Float.float2Double $ amplitude_fv
    let ct = Vector.length frequency_dv `div` 2
    return . fmap GHC.Float.double2Float . Vector.take ct $ frequency_dv

wavg :: Num n => n -> n -> n -> n
wavg w a b = a*w + b*(1-w)

-- eof
