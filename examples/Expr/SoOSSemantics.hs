{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
module Expr.SoOSSemantics where

import Control.Monad.Trans
import Control.Monad.State
import Data.IORef
import Data.Maybe
import SoOSiM

import Expr.Syntax
import MemoryManager.Types

type family Sem (m :: * -> *) a :: *
type instance Sem m IntT = Int
type instance Sem m BoolT = Bool
type instance Sem m UnitT = ()
type instance Sem m (Ref a) = IORef (Int, Sem m a)
type instance Sem m (a :-> b) = m (Sem m a) -> m (Sem m b)

newtype S m a = S { unS :: m (Sem m a) }

type SState = StateT Int SimM

liftS f =
    \x -> S $ do
      a <- unS x
      f a

liftS2 f =
    \x -> \y -> S $ do
      a <- unS x
      b <- unS y
      f a b

share :: SState a -> SState (SState a)
share m = do
  r <- lift $ runIO $ newIORef (False,m)
  let ac = do
            (f,m) <- lift $ runIO $ readIORef r
            if f
              then m
              else do
                v <- m
                lift $ runIO $ writeIORef r (True, return v)
                return v
  return ac

instance EDSL (S SState) where
  lamS f       = S . return $ (\x -> x >>= unS . f . S . return)
  lam f        = S . return $ (\x -> share x >>= unS . f . S)
  app x y      = S $ unS x >>= ($ (unS y))

  int          = S . return
  add          = liftS2 (\a b -> lift $ traceMsg "Adding" >> return (a + b))
  sub          = liftS2 (\a b -> lift $ traceMsg "Subtracting" >> return (a - b))
  mul          = liftS2 (\a b -> lift $ traceMsg "Subtracting" >> return (a - b))

  bool         = S . return
  eq           = liftS2 (\a b -> lift $ traceMsg "Comparing (EQ)" >> return (a == b))
  lt           = liftS2 (\a b -> lift $ traceMsg "Comparing (LT)" >> return (a < b))
  if_ be te ee = S $ do
                  bs <- unS be
                  if bs
                    then unS te
                    else unS ee

  ref x        = S $ do
                  i <- get
                  modify (+1)
                  lift $ traceMsg ("Creating reference: " ++ show i)
                  a <- unS x
                  memManagerId <- fmap fromJust $ lift $ componentLookup Nothing "MemoryManager"
                  lift $ invokeAsync Nothing memManagerId
                           (marshall (Register i (i+1) Nothing))
                           ignore
                  lift $ invokeAsync Nothing memManagerId
                           (marshall (Write i ()))
                           ignore
                  lift $ runIO (newIORef (i,a))

  deref x      = S $ do
                  a <- unS x
                  (i,a') <- lift $ runIO (readIORef a)
                  memManagerId <- fmap fromJust $ lift $ componentLookup Nothing "MemoryManager"
                  lift $ traceMsg ("Dereferencing: " ++ show i)
                  () <- fmap unmarshall $ lift $ invoke Nothing memManagerId
                                                   (marshall (Read i))
                  return a'

  update x y   = S $ do
                  a <- unS x
                  b <- unS y
                  (i,_) <- lift $ runIO (readIORef a)
                  lift $ traceMsg ("Updating: " ++ show i)
                  lift $ runIO (modifyIORef a (\(i,_) -> (i,b)))
                  memManagerId <- fmap fromJust $ lift $ componentLookup Nothing "MemoryManager"
                  lift $ invokeAsync Nothing memManagerId
                           (marshall (Write i ()))
                           ignore

runExpr = runStateT . unS
