{-# LANGUAGE ScopedTypeVariables #-}
module SoOSiM
  ( SimM
  , ComponentId
  , NodeId
  , ComponentIface (..)
  , ComponentInput (..)
  , module SoOSiM.SimMonad
  , Dynamic
  , Typeable
  , marshall
  , safeUnmarshall
  , unmarshall
  , ignore
  )
where

import Data.Dynamic

import SoOSiM.SimMonad
import SoOSiM.Types

marshall :: Typeable a => a -> Dynamic
marshall = toDyn

safeUnmarshall :: Typeable a => Dynamic -> Maybe a
safeUnmarshall = fromDynamic

unmarshall :: forall a . Typeable a => Dynamic -> a
unmarshall d = fromDyn d
                 (error $  "unmarshal failed: expected value of type: "
                        ++ show resDyn
                        ++ ", received value of type: "
                        ++ show d
                 )
  where
    resDyn :: Dynamic
    resDyn = toDyn (undefined :: a)

ignore :: Dynamic -> SimM ()
ignore = const (return ())
