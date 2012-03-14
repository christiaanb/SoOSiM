module SoOSiM.CoroutineT where

import Control.Monad.Trans.Class

data Result i m a = Yield Int (i -> CoroutineT i m a) | Result a

data CoroutineT i m a = CoroutineT { runCoroutineT :: m (Result i m a) }

yield :: Monad m => Int -> CoroutineT i m i
yield o = CoroutineT $ return $ Yield o (\i -> CoroutineT $ return $ Result i)

instance Monad m => Monad (CoroutineT i m) where
  return a = CoroutineT $ return $ Result a
  f >>= g  = CoroutineT $ do
    res1 <- runCoroutineT f
    case res1 of
      Yield o c -> return $ Yield o (\i -> c i >>= g)
      Result a  -> runCoroutineT (g a)
  fail err = CoroutineT $ fail err

instance MonadTrans (CoroutineT i) where
  lift m = CoroutineT (m >>= (return . Result))
