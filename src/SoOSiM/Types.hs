{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module SoOSiM.Types where

import           Control.Concurrent.STM     (TVar)
import           Control.Concurrent.Supply  (Supply,freshId)
import           Control.Monad.Coroutine    (Coroutine)
import qualified Control.Monad.State        as State
import           Data.Dynamic               (Dynamic)
import           Data.IntMap                (IntMap)
import           Data.Map                   (Map)

import           SoOSiM.Util                (MonadUnique(..))

type Unique        = Int
type ComponentId   = Unique
type ComponentName = String

-- | Type class that defines every OS component
class ComponentIface s where
  type Send    s
  type Receive s
  type State   s
  -- | The minimal internal state of your component
  initState          :: s -> State s
  -- | A function returning the unique global name of your component
  componentName      :: s -> ComponentName
  -- | The function defining the behaviour of your component
  componentBehaviour :: s -> State s -> Input (Receive s) -> SimM (State s)

-- | Context of a running component in the simulator.
--
-- We need existential types because we need to make a single collection
-- of several component contexts, each having their own type representing
-- their internal state.
data ComponentContext = forall s . ComponentIface s =>
  CC { componentId        :: ComponentId
     , currentStatus      :: TVar (ComponentStatus s)
     -- ^ Status of the component
     , componentState     :: TVar s
     -- ^ State internal to the component
     , creator            :: ComponentId
     -- ^ 'ComponentId' of the component that created this component
     , msgBuffer          :: TVar [Input Dynamic]
     -- ^ Message waiting to be processed by the component
     , traceMsgs          :: [String]
     -- ^ Trace message buffer
     , simMetaData        :: TVar SimMetaData
     -- ^ Statistical information regarding a component
     }

data SimMetaData
  = SimMetaData
  { cyclesRunning :: Int
  , cyclesWaiting :: Int
  , cyclesIdling  :: Int
  , msgsReceived  :: Map ComponentId Int
  -- ^ Key: senderId; Value: number of messages
  , msgsSend      :: Map ComponentId Int
  -- ^ Key: receiverId: Value: number of messages
  }

-- | Status of a running component
data ComponentStatus a
  = Idle
  -- ^ Component is doing nothing
  | WaitingForMsg ComponentId (() -> SimM a)
  -- ^ Component is waiting for a message from 'ComponentId', will continue
  -- with computation ('(' -> 'SimM' a) once received
  | Running
  -- ^ Component is busy doing computations

-- | Events send to components by the simulator
data Input a
  = Message a ReturnChannel
  -- ^ A message send another component: the field argument is the
  -- 'ComponentId' of the sender, the second field the message content
  | Tick
  -- ^ Event send every simulation round

newtype ReturnChannel = RC { unRC :: (ComponentId, TVar Dynamic) }

type NodeId   = Unique
-- | Meta-data describing the functionaly of the computing node, currently
-- just a singleton type.
data NodeInfo = NodeInfo

-- | Nodes represent computing entities in the simulator,
-- and host the OS components and application threads
data Node
  = Node
  { nodeId              :: NodeId
  -- ^ Globally Unique ID of the node
  , nodeInfo            :: NodeInfo
  -- ^ Meta-data describing the node
  , nodeComponentLookup :: Map ComponentName ComponentId
  -- ^ Lookup table of OS components running on the node, key: the
  -- 'ComponentName', value: unique 'ComponentId'
  , nodeComponents      :: IntMap ComponentContext
  -- ^ Map of component contexts, key is the 'ComponentId'
  , nodeMemory          :: IntMap Dynamic
  -- ^ Node-local memory
  , nodeComponentOrder  :: [ComponentId]
  }

-- | The simulator monad used by the OS components offers resumable
-- computations in the form of coroutines. These resumable computations
-- expect a value of type 'Dynamic', and return a value of type 'a'.
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
newtype SimM a = SimM { runSimM ::
                          Coroutine
                            (RequestOrYield Unique ())
                            SimMonad
                            a
                      }
  deriving (Functor, Monad)

data RequestOrYield request response x
  = Request request (response -> x)
  | Yield   x

instance Functor (RequestOrYield x f) where
  fmap f (Request x g) = Request x (f . g)
  fmap f (Yield y)     = Yield (f y)

-- | The internal monad of the simulator is currently a simple state-monad
-- wrapping IO
type SimMonad  = State.StateT SimState IO

-- | The internal simulator state
data SimState
  = SimState
  { currentComponent :: ComponentId
  -- ^ The 'ComponentId' of the component currently under evaluation
  , currentNode      :: NodeId
  -- ^ The 'NodeId' of the node containing the component currently under
  -- evaluation
  , nodes            :: IntMap Node
  -- ^ The set of nodes comprising the entire system
  , uniqueSupply     :: Supply
  -- ^ Unlimited supply of unique values
  }

instance MonadUnique SimMonad where
  getUniqueM = do
    supply <- State.gets uniqueSupply
    let (unique,supply') = freshId supply
    State.modify (\s -> s {uniqueSupply = supply'})
    return unique
