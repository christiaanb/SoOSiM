{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
module Expr.SimpleSemantics where

import Control.Monad.Trans
import Control.Monad.State
import Data.IORef

import Expr.Syntax

type family Sem (m :: * -> *) a :: *
type instance Sem m IntT = Int
type instance Sem m BoolT = Bool
type instance Sem m UnitT = ()
type instance Sem m (Ref a) = IORef (Int, Sem m a)
type instance Sem m (a :-> b) = m (Sem m a) -> m (Sem m b)

newtype S m a = S { unS :: m (Sem m a) }

liftS f =
    \x -> S $ do
      a <- unS x
      f a

liftS2 f =
    \x -> \y -> S $ do
      a <- unS x
      b <- unS y
      f a b

share :: MonadIO m => m a -> m (m a)
share m = do
  r <- liftIO $ newIORef (False,m)
  let ac = do
            (f,m) <- liftIO $ readIORef r
            if f
              then m
              else do
                v <- m
                liftIO $ writeIORef r (True, return v)
                return v
  return ac

type SState = StateT Int IO

instance EDSL (S SState) where
  lamS f       = S . return $ (\x -> x >>= unS . f . S . return)
  lam f        = S . return $ (\x -> share x >>= unS . f . S)
  app x y      = S $ unS x >>= ($ (unS y))

  int          = S . return
  add          = liftS2 (\a b -> liftIO (putStrLn "Adding") >> return (a + b))
  sub          = liftS2 (\a b -> liftIO (putStrLn "Subtracting") >> return (a - b))
  mul          = liftS2 (\a b -> liftIO (putStrLn "Subtracting") >> return (a - b))

  bool         = S . return
  eq           = liftS2 (\a b -> liftIO (putStrLn "Comparing (EQ)") >> return (a == b))
  lt           = liftS2 (\a b -> liftIO (putStrLn "Comparing (LT)") >> return (a < b))
  if_ be te ee = S $ do
                  bs <- unS be
                  if bs
                    then unS te
                    else unS ee

  ref x        = S $ do
                  i <- get
                  modify (+1)
                  liftIO $ putStrLn ("Creating reference: " ++ show i)
                  a <- unS x
                  liftIO (newIORef (i,a))

  deref x      = S $ do
                  a <- unS x
                  (i,a') <- liftIO (readIORef a)
                  liftIO $ putStrLn ("Dereferencing: " ++ show i)
                  return a'

  update x y   = S $ do
                  a <- unS x
                  b <- unS y
                  (i,_) <- liftIO (readIORef a)
                  liftIO $ putStrLn ("Updating: " ++ show i)
                  liftIO (modifyIORef a (\(i,_) -> (i,b)))

runExpr e = evalStateT (unS e) (0 :: Int) >>= print
