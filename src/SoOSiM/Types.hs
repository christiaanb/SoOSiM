{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
module SoOSiM.Types where

import Control.Monad.State
import Data.Dynamic
import Data.IntMap

import SoOSiM.CoroutineT

type ComponentId = Int

type SimM a = CoroutineT Dynamic SimMonad a
type SimMonad = StateT SimState IO

data SimState =
  SimState { currentComponent :: ComponentId
           , currentNode      :: NodeId
           , nodes            :: IntMap Node
           }

class ComponentIface s where
  initState    :: s
  componentFun :: s -> ComponentInput -> SimM s

data NodeInfo = NodeInfo
data ComponentInput = ComponentMsg ComponentId Dynamic
                    | NodeMsg NodeId Dynamic

data ComponentStatus a = Idle | WaitingForMsg ComponentId (Dynamic -> SimM a) | Running

type NodeId = Int

data ContainerElement s =
  CE { currentStatus :: ComponentStatus s
     , currentState  :: s
     , creator       :: ComponentId
     , msgBuffer     :: [ComponentInput]
     , compFun       :: s -> ComponentInput -> SimM s
     }

data ComponentContainer = forall s . ComponentIface s => CC (IntMap (ContainerElement s))

data Node =
  Node { nodeId         :: NodeId
       , nodeInfo       :: NodeInfo
       , nodeComponents :: ComponentContainer
       , nodeMemory     :: IntMap Dynamic
       }

data MsgMode = Sync | Async

data MemCommand = Read Int
                | Write Int Dynamic
  deriving Typeable
