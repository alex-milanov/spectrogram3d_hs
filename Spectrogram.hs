-- qualified

import qualified Control.Exception as Exc
import qualified System.Environment as Env
import qualified Data.IORef as IORef
import qualified AudioMain
import qualified DisplayMain

-- unqualified

import Control.Concurrent (forkFinally)
import Text.Printf (printf)

main :: IO ()
main = do
    name <- Env.getProgName
    (stride, offset) <- Env.getArgs >>= parse_args
    -- audio thread
    raudst <- IORef.newIORef (AudioMain.initial stride)
    forkFinally (AudioMain.audio_main raudst stride offset)
                $ either (\ex -> printf "audio monitor thread crashed (%s)\n" $ show ex)
                         (\_ -> printf "audio monitor thread gracefully\n")
    -- display thread (this thread)
    let (dispst, dispconf) = DisplayMain.initial name 40 0.8
    rdispst <- IORef.newIORef dispst
    Exc.catch (DisplayMain.display_main dispconf rdispst raudst)
              $ \ex -> printf "graphics mainloop crashed (%s)\n" $ show (ex :: Exc.SomeException)
    printf "graphics mainloop finished\n"

parse_args :: [String] -> IO (Int, Int)
parse_args argv = do
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

-- eof
