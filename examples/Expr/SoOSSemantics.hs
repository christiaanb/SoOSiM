{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
module Expr.SoOSSemantics where

import Control.Concurrent.STM
import Control.Monad.Trans
import Control.Monad.State
import Data.Maybe
import SoOSiM

import Expr.Syntax
import MemoryManager
import MemoryManager.Types

type family Sem (m :: * -> *) a :: *
type instance Sem m IntT = Int
type instance Sem m BoolT = Bool
type instance Sem m UnitT = ()
type instance Sem m (Ref a) = TVar (Int, Sem m a)
type instance Sem m (a :-> b) = m (Sem m a) -> m (Sem m b)

newtype S m a = S { unS :: m (Sem m a) }

type SState = StateT Int Sim

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
  r <- lift $ runSTM $ newTVar (False,m)
  let ac = do
            (f,m) <- lift $ runSTM $ readTVar r
            if f
              then m
              else do
                v <- m
                lift $ runSTM $ writeTVar r (True, return v)
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
                  memManagerId <- fmap fromJust $ lift $ componentLookup Nothing MemoryManager
                  lift $ invokeAsync MemoryManager Nothing memManagerId
                           (Register i (i+1) Nothing) ignore
                  lift $ invokeAsync MemoryManager Nothing memManagerId
                           (Write i ()) ignore
                  lift $ runSTM (newTVar (i,a))

  deref x      = S $ do
                  a <- unS x
                  (i,a') <- lift $ runSTM (readTVar a)
                  memManagerId <- fmap fromJust $ lift $ componentLookup Nothing MemoryManager
                  lift $ traceMsg ("Dereferencing: " ++ show i)
                  () <- fmap (unmarshall "deref") $ lift $
                          invoke MemoryManager Nothing memManagerId (Read i)
                  return a'

  update x y   = S $ do
                  a <- unS x
                  b <- unS y
                  (i,_) <- lift $ runSTM (readTVar a)
                  lift $ traceMsg ("Updating: " ++ show i)
                  lift $ runSTM (modifyTVar a (\(i,_) -> (i,b)))
                  memManagerId <- fmap fromJust $ lift $ componentLookup Nothing MemoryManager
                  lift $ invokeAsync MemoryManager Nothing memManagerId
                           (Write i ()) ignore

runExpr = runStateT . unS
