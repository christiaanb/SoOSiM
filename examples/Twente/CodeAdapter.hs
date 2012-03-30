{-# LANGUAGE DeriveDataTypeable #-}
module CodeAdapter where

import Control.Monad.IfElse

import SoOSiM
import Code
import Node

data TransfomerState = TransfomerState

data CodeAdapterMsg = Compile BlockCode Architecture
 deriving (Typeable)

instance ComponentIface TransfomerState where

 initState          = TransfomerState

 componentName _    = "CodeAdapter"

 componentBehaviour state (ComponentMsg caller contents)
  | Just (Compile code node) <- adapterAction
  -- To be completed
  = return state
  where adapterAction = fromDynamic contents
 componentBehaviour state _ = return state
