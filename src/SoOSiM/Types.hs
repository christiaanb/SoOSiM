{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SoOSiM.Types where

import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Monad.State
import Control.Monad.Trans.Class ()
import Data.Dynamic
import Data.IntMap
import Data.Map

newtype SimM a = SimM { runSimM :: Coroutine (Request Int Dynamic) SimMonad a }
  deriving Monad

type SimMonad  = StateT SimState IO

data SimState =
  SimState { currentComponent :: ComponentId
           , currentNode      :: NodeId
           , nodes            :: IntMap Node
           }

class ComponentIface s where
  initState    :: s
  componentFun :: s -> ComponentInput -> SimM s

type ComponentId   = Int
type ComponentName = String

data ComponentInput = ComponentMsg ComponentId Dynamic
                    | NodeMsg NodeId Dynamic

data ComponentStatus a = Idle | WaitingForMsg ComponentId (Dynamic -> SimM a) | Running

data ComponentContext s =
  CE { currentStatus  :: ComponentStatus s
     , componentState :: s
     , creator        :: ComponentId
     , msgBuffer      :: [ComponentInput]
     , componentName  :: ComponentName
     , compFun        :: s -> ComponentInput -> SimM s
     }

data ComponentContainer = forall s . ComponentIface s => CC (IntMap (ComponentContext s))

type NodeId   = Int
data NodeInfo = NodeInfo

data Node =
  Node { nodeId              :: NodeId
       , nodeInfo            :: NodeInfo
       , nodeComponentLookup :: Map ComponentName ComponentId
       , nodeComponents      :: ComponentContainer
       , nodeMemory          :: IntMap Dynamic
       }

data MemCommand = Read Int
                | Write Int Dynamic
  deriving Typeable
