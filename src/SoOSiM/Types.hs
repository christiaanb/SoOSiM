{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
module SoOSiM.Types where

import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Monad.State
import Control.Monad.Trans.Class ()
import Data.Dynamic
import Data.IntMap
import Data.Map
import UniqSupply

newtype SimM a = SimM { runSimM :: Coroutine (Request Int Dynamic) SimMonad a }
  deriving Monad

type SimMonad  = StateT SimState IO

data SimState =
  SimState { currentComponent :: ComponentId
           , currentNode      :: NodeId
           , nodes            :: IntMap Node
           , uniqueSupply     :: UniqSupply
           }

instance MonadUnique SimMonad where
  getUniqueSupplyM = gets uniqueSupply

class ComponentIface s where
  initState          :: s
  componentName      :: s -> ComponentName
  componentBehaviour :: s -> ComponentInput -> SimM s

type ComponentId   = Int
type ComponentName = String

data ComponentInput = ComponentMsg ComponentId Dynamic
                    | NodeMsg NodeId Dynamic
                    | Initialize
                    | Deinitialize

data ComponentStatus a = Idle | WaitingForMsg ComponentId (Dynamic -> SimM a) | Running

data ComponentContext = forall s . ComponentIface s =>
  CC { currentStatus      :: ComponentStatus s
     , componentState     :: s
     , creator            :: ComponentId
     , msgBuffer          :: [ComponentInput]
     }

type NodeId   = Int
data NodeInfo = NodeInfo

data Node =
  Node { nodeId              :: NodeId
       , nodeInfo            :: NodeInfo
       , nodeComponentLookup :: Map ComponentName ComponentId
       , nodeComponents      :: IntMap ComponentContext
       , nodeMemory          :: IntMap Dynamic
       }

data MemCommand = Read Int
                | Write Int Dynamic
  deriving Typeable
