{-# LANGUAGE NoMonomorphismRestriction #-}
module Expr.Combinators where

import Expr.Syntax

f $$ a = f `app` a

infixl 2 $$

let_ x y = (lam y)  `app` x
letS x y = (lamS y) `app` x

newvar x y = letS (ref x) y
x >: y     = letS x (\_ -> y)

newIVar y  = letS iVar y

infixr 5 >:

fix' = lam $ \f ->
         newvar (lam $ \r -> undefined) $ \r ->
           update r (lam $ \x -> f $$ (deref r) $$ x) >:
           deref r

fix f = app fix' (lam f)
