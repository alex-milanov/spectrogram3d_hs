module Sound.Signal.Util where

import qualified Control.Monad as Monad

-- Conditional execution with a result.
when :: Monad.MonadPlus m => Bool -> a -> m a
when b x = do Monad.guard b
              return x

-- eof
