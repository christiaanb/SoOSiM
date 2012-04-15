{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Fibbo where

import SoOSiM

import Expr.Syntax
import Expr.Combinators
-- import Expr.SimpleSemantics
import Expr.SoOSSemantics

fibbo = fix $ \fib ->
  lam $ \n ->
    newvar 0 $ \n1 ->
    newvar 0 $ \n2 ->
    newvar 0 $ \n3 ->
      update n1 n >:
      if_ (lt (deref n1) 2)
        1
        ( update n2 (app fib ((deref n1) - 1)) >:
          update n3 (app fib ((deref n1) - 2)) >:
          (deref n2) + (deref n3)
        )

fibbo5 :: EDSL exp => exp IntT
fibbo5 = fibbo $$ 5

newtype FS = FS Int

fibbo5Component ::
  FS
  -> ComponentInput
  -> SimM FS
fibbo5Component (FS s) Initialize = do
  (a,s') <- runExpr fibbo5 s
  traceMsg ("Result: " ++ show a)
  yield (FS s')

instance ComponentIface FS where
  initState          = FS 0
  componentName _    = "Fibbo5"
  componentBehaviour = fibbo5Component
