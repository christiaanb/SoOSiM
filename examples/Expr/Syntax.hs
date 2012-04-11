{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleInstances #-}
module Expr.Syntax where

data IntT
data BoolT
data UnitT
data Ref a
data a :-> b
infixr 5 :->

class EDSL exp where
     lam :: (exp a -> exp b) -> exp (a :-> b)
     lam = lamS

     lamS :: (exp a -> exp b) -> exp (a :-> b)
     app :: exp (a :-> b) -> exp a -> exp b

     int :: Int -> exp IntT
     add :: exp IntT -> exp IntT -> exp IntT
     sub :: exp IntT -> exp IntT -> exp IntT
     mul :: exp IntT -> exp IntT -> exp IntT

     bool :: Bool -> exp BoolT
     eq :: exp IntT -> exp IntT -> exp BoolT
     lt :: exp IntT -> exp IntT -> exp BoolT
     if_ :: exp BoolT -> exp a -> exp a -> exp a

     ref :: exp a -> exp (Ref a)
     deref :: exp (Ref a) -> exp a
     update :: exp (Ref a) -> exp a -> exp UnitT

instance EDSL exp => Num (exp IntT) where
  fromInteger = int . fromInteger
  (+)         = add
  (-)         = sub
  (*)         = mul
  abs         = error "abs undefined for EDSL"
  signum      = error "signum undefined for EDSL"
