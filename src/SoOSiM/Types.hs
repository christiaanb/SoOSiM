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
import Unique
import UniqSupply

type ComponentId   = Unique
type ComponentName = String

-- | Type class that defines every OS component
class ComponentIface s where
  -- | The minimal internal state of your component
  initState          :: s
  -- | A function returning the unique global name of your component
  componentName      :: s -> ComponentName
  -- | The function defining the behaviour of your component
  componentBehaviour :: s -> ComponentInput -> SimM s

-- | Context of a running component in the simulator.
--
-- We need rank-2 types because we need to make a single collection
-- of several component contexts, each having their own type representing
-- their internal state.
data ComponentContext = forall s . ComponentIface s =>
  CC { currentStatus      :: ComponentStatus s  -- ^ Status of the component
     , componentState     :: s                  -- ^ State internal to the component
     , creator            :: ComponentId        -- ^ 'ComponentId' of the component that created this component
     , msgBuffer          :: [ComponentInput]   -- ^ Message waiting to be processed by the component
     }

-- | Status of a running component
data ComponentStatus a
  = Idle                                          -- ^ Component is doing nothing
  | WaitingForMsg ComponentId (Dynamic -> SimM a) -- ^ Component is waiting for a message from 'ComponentId', will continue with computation ('Dynamic' -> 'SimM' a) once received
  | Running                                       -- ^ Component is busy doing computations


-- | Events send to components by the simulator
data ComponentInput = ComponentMsg ComponentId Dynamic -- ^ A message send another component: the field argument is the 'ComponentId' of the sender, the second field the message content
                    | NodeMsg NodeId Dynamic           -- ^ A message send by a node: the first field is the 'NodeId' of the sending node, the second field the message content
                    | Initialize                       -- ^ Event send when a component is first created
                    | Deinitialize                     -- ^ Event send when a component is about to be removed

type NodeId   = Unique
-- | Meta-data describing the functionaly of the computing node, currently just a singleton type.
data NodeInfo = NodeInfo

-- | Nodes represent computing entities in the simulator,
-- and host the OS components and application threads
data Node =
  Node { nodeId              :: NodeId                        -- ^ Globally Unique ID of the node
       , nodeInfo            :: NodeInfo                      -- ^ Meta-data describing the node
       , nodeComponentLookup :: Map ComponentName ComponentId -- ^ Lookup table of OS components running on the node, key: the 'ComponentName', value: unique 'ComponentId'
       , nodeComponents      :: IntMap ComponentContext       -- ^ Map of component contexts, key is the 'ComponentId'
       , nodeMemory          :: IntMap Dynamic                -- ^ Node-local memory
       }

-- The simulator monad used by the OS components offers resumable computations
-- in the form of coroutines. These resumable computations expect a value of
-- type 'Dynamic', and return a value of type 'a'.
--
-- We need resumable computations to simulate synchronous messaging between
-- two components. When a component synchronously sends a message to another
-- component, we store the rest of the computation as part of the execution
-- context in the simulator state. When a message is send back, the stored
-- computation will continue with the message content (of type 'Dynamic').
--
-- To suspend a computation you simply do:
--   'request <componentId>'
--
-- Where the <componentId> is the ID of the OS component you are expecting a
-- message from. The execute a resumeable computation you simply do:
--   'resume <comp>'
--
newtype SimM a = SimM { runSimM :: Coroutine (Request Unique Dynamic) SimMonad a }
  deriving Monad

-- | The internal monad of the simulator is currently a simple state-monad wrapping IO
type SimMonad  = StateT SimState IO

-- | The internal simulator state
data SimState =
  SimState { currentComponent :: ComponentId  -- ^ The 'ComponentId' of the component currently under evaluation
           , currentNode      :: NodeId       -- ^ The 'NodeId' of the node containing the component currently under evaluation
           , nodes            :: IntMap Node  -- ^ The set of nodes comprising the entire system
           , uniqueSupply     :: UniqSupply   -- ^ Unlimited supply of unique values
           }

instance MonadUnique SimMonad where
  getUniqueSupplyM = gets uniqueSupply
