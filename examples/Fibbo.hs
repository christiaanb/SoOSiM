{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
module Fibbo where

import SoOSiM

import Expr.Syntax
import Expr.Combinators
-- import Expr.SimpleSemantics
import Expr.SoOSSemantics

fibbo = fix $ \fib ->
  lam $ \n ->
    newvar 0 $ \n1 ->
    newIVar $ \n2 ->
    newIVar $ \n3 ->
      update n1 n >:
      if_ (lt (deref n1) 2)
        1
        ( par (wrIVar n2 (app fib ((deref n1) - 1))) >:
          par (wrIVar n3 (app fib ((deref n1) - 2))) >:
          (rdIVar n2) + (rdIvar n3)
        )

fibbo5 :: EDSL exp => exp IntT
fibbo5 = fibbo $$ 5

fibbo5Component ::
  Int
  -> Input ()
  -> Sim Int
fibbo5Component s (Message () _) = do
  (a,s') <- runExpr fibbo5 s
  traceMsg ("Result: " ++ show a)
  yield s'

data Fibbo = Fibbo

instance ComponentInterface Fibbo where
  type State Fibbo     = Int
  type Receive Fibbo   = ()
  type Send Fibbo      = ()
  initState _          = 0
  componentName _      = "Fibbo5"
  componentBehaviour _ = fibbo5Component
