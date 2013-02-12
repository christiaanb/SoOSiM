{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module SoOSiM.SimMonad
  ( -- * Basic API
    createComponent
  , invoke
  , invokeAsync
  , notify
  , respond
  , yield
  , readMemory
  , writeMemory
  , componentLookup
  , traceMsg
  , traceMsgTag
  , createNode
  , compute
  , stop
  -- * Advanced API
  , runSTM
  , getComponentId
  , getNodeId
  , componentCreator
  , getTime
  , stopSim
  -- * Specialized API
  , createComponentN
  , createComponentNP
  , createComponentNPS
  , invokeS
  , invokeAsyncS
  , notifyS
  , respondS
  , readMemoryN
  , writeMemoryN
  )
where

import           Control.Concurrent.STM  (STM,newTVar)
import           Control.Monad.Coroutine (suspend)
import           Control.Monad.State     (gets,lift,modify)
import           Data.Dynamic            (Dynamic,Typeable,toDyn)
import qualified Data.IntMap             as IM
import qualified Data.Map                as Map
import           Data.Maybe              (fromMaybe)

import SoOSiM.Simulator.Util
import SoOSiM.Types
import SoOSiM.Util

{-# INLINE createComponent #-}
-- | Create a new component
createComponent ::
  (ComponentInterface iface, Typeable (Receive iface))
  => iface
  -- ^ Component Interface
  -> Sim ComponentId
  -- ^ 'ComponentId' of the created component
createComponent = createComponentNPS Nothing Nothing Nothing

{-# INLINE createComponentN #-}
-- | Create a new component
createComponentN ::
  (ComponentInterface iface, Typeable (Receive iface))
  => iface
  -- ^ Component Interface
  -> NodeId
  -- Node to create component on
  -> Sim ComponentId
createComponentN iface nId =
  createComponentNPS (Just nId) Nothing Nothing iface

{-# INLINE createComponentNP #-}
-- | Create a new component
createComponentNP ::
  (ComponentInterface iface, Typeable (Receive iface))
  => NodeId
  -- ^ Node to create component on, leave to 'Nothing' to create on current
  -- node
  -> ComponentId
  -- ^ ComponentId to set as parent, set to 'Nothing' to use own ComponentId
  -> iface
  -- ^ Component Interface
  -> Sim ComponentId
  -- ^ 'ComponentId' of the created component
createComponentNP nodeId parentId iface =
  createComponentNPS (Just nodeId) (Just parentId) Nothing iface

-- | Create a new component
createComponentNPS ::
  (ComponentInterface iface, Typeable (Receive iface))
  => Maybe NodeId
  -- ^ Node to create component on, leave to 'Nothing' to create on current
  -- node
  -> Maybe ComponentId
  -- ^ ComponentId to set as parent, set to 'Nothing' to use own ComponentId
  -> Maybe (State iface)
  -- ^ Internal State, leave 'Nothing' to set to default
  -> iface
  -- ^ Component Interface
  -> Sim ComponentId
  -- ^ 'ComponentId' of the created component
createComponentNPS nodeIdM parentIdM iStateM iface = Sim $ do
    nodeId    <- fmap (`fromMaybe` nodeIdM) $ gets currentNode
    parentId  <- fmap (`fromMaybe` parentIdM) $ gets currentComponent
    compId    <- getUniqueM

    statusTV  <- (lift . lift) $ newTVar ReadyToRun

    let iState = fromMaybe (initState iface) iStateM
    stateTV   <- (lift . lift) $ newTVar iState
    msgBufTV  <- (lift . lift) $ newTVar []
    let meta  = SimMetaData 0 0 0 Map.empty Map.empty
    metaTV    <- (lift . lift) $ newTVar meta

    let component = (CC iface compId parentId statusTV stateTV msgBufTV [] metaTV)

    lift $ modifyNode nodeId (addComponent compId component)

    return compId
  where
    cname = componentName iface

    addComponent cId comp n@(Node {..}) =
      n { nodeComponents = IM.insert cId comp nodeComponents
        , nodeComponentLookup = Map.insert cname cId nodeComponentLookup
        }

{-# INLINE invoke #-}
-- | Synchronously invoke another component
invoke ::
  (ComponentInterface iface, Typeable (Receive iface), Typeable (Send iface))
  => iface
  -- ^ Interface type
  -> ComponentId
  -- ^ ComponentId of callee
  -> Receive iface
  -- ^ Argument
  -> Sim (Send iface)
  -- ^ Response from callee
invoke iface recipient content = invokeS iface Nothing recipient content

-- | Synchronously invoke another component
invokeS ::
  forall iface
  . (ComponentInterface iface
    , Typeable (Receive iface)
    , Typeable (Send iface))
  => iface
  -- ^ Interface type
  -> Maybe ComponentId
  -- ^ Caller, leave 'Nothing' to set to current module
  -> ComponentId
  -- ^ Callee
  -> Receive iface
  -- ^ Argument
  -> Sim (Send iface)
  -- ^ Response from recipient
invokeS _ senderM recipient content = Sim $ do
  t            <- gets simClk
  sender       <- fmap (`fromMaybe` senderM) $ gets currentComponent
  let message  = Message t (toDyn content) (RA (sender,sender))
  lift $ sendMessage sender recipient message
  var <- suspend (Request recipient return)
  return (unmarshall "invoke" var)

{-# INLINE invokeAsync #-}
-- | Invoke another component, handle response asynchronously
invokeAsync ::
  (ComponentInterface iface, Typeable (Receive iface), Typeable (Send iface))
  => iface
  -- ^ Interface type
  -> ComponentId
  -- ^ ComponentId of callee
  -> Receive iface
  -- ^ Argument
  -> (Send iface -> Sim ())
  -- ^ Response Handler
  -> Sim ()
  -- ^ Call returns immediately
invokeAsync iface recipient content handler =
  invokeAsyncS iface Nothing recipient content handler

-- | Invoke another component, handle response asynchronously
invokeAsyncS ::
  forall iface
  . (ComponentInterface iface
    , Typeable (Receive iface)
    , Typeable (Send iface))
  => iface
  -- ^ Interface type
  -> Maybe ComponentId
  -- ^ Parent of handler, leave 'Nothing' to set to the current module
  -> ComponentId
  -- ^ Callee
  -> (Receive iface)
  -- ^ Argument
  -> (Send iface -> Sim ())
  -- ^ Handler
  -> Sim ()
  -- ^ Call returns immediately
invokeAsyncS _ parentIdM recipient content handler = Sim $ do
  nodeId       <- gets currentNode
  parentId     <- fmap (`fromMaybe` parentIdM) $ gets currentComponent
  sender       <- runSim $ createComponentNPS (Just nodeId) parentIdM
                    (Just (recipient,handler . unmarshallAsync))
                    (HS parentId)
  t            <- gets simClk

  let message  = Message t (toDyn content) (RA (sender,parentId))
  lift $ sendMessage parentId recipient message

  where
    unmarshallAsync :: Dynamic -> Send iface
    unmarshallAsync = unmarshall "invokeAsyncS"

{-# INLINE notify #-}
-- | Notify another component
notify ::
  (ComponentInterface iface, Typeable (Receive iface))
  => iface
  -- ^ Interface type
  -> ComponentId
  -- ^ ComponentId of callee
  -> Receive iface
  -- ^ Argument
  -> Sim ()
notify iface recipient content = notifyS iface Nothing recipient content

-- | Notify another component
notifyS ::
  forall iface
  . (ComponentInterface iface
    , Typeable (Receive iface))
  => iface
  -- ^ Interface type
  -> Maybe ComponentId
  -- ^ Caller, leave 'Nothing' to set to current module
  -> ComponentId
  -- ^ Callee
  -> Receive iface
  -- ^ Argument
  -> Sim ()
notifyS _ senderM recipient content = Sim $ do
  sender       <- fmap (`fromMaybe` senderM) $ gets currentComponent
  t            <- gets simClk
  let message  = Message t (toDyn content) (RA (sender,sender))
  lift $ sendMessage sender recipient message

{-# INLINE respond #-}
-- | Respond to an invocation
respond ::
  (ComponentInterface iface, Typeable (Send iface))
  => iface
  -- ^ Interface type
  -> ReturnAddress
  -- ^ Return address to send response to
  -> (Send iface)
  -- ^ Value to send as response
  -> Sim ()
  -- ^ Call returns immediately
respond iface retAddr content = respondS iface Nothing retAddr content

-- | Respond to an invocation
respondS ::
  forall iface
  . ( ComponentInterface iface
    , Typeable (Send iface))
  => iface
  -- ^ Interface type
  -> Maybe ComponentId
  -- ^ Callee Id, leave 'Nothing' to set to current module
  -> ReturnAddress
  -- ^ Return address
  -> (Send iface)
  -- ^ Value to send as response
  -> Sim ()
  -- ^ Call returns immediately
respondS _ senderM recipient content = Sim $ do
  sender <- fmap (`fromMaybe` senderM) $ gets currentComponent
  t      <- gets simClk
  let message = Message t (toDyn content) (RA (sender,sender))
  lift $ sendMessage sender (fst $ unRA recipient) message

-- | Have a pure computation run for 'n' simulator ticks
compute ::
  Int       -- ^ The number of ticks the computation should take
  -> a      -- ^ The pure computation
  -> Sim a  -- ^ Result of the pure computation
compute i a
  | i < 1     = return a
  | otherwise = Sim $ suspend (Run i (return a))

-- | Yield internal state to the simulator scheduler
yield ::
  a
  -> Sim a
yield s = Sim $ suspend (Yield (return s))

-- | Stop the component
-- NB I cannot be resumed!
stop ::
  Sim a
stop = Sim $ suspend Kill

-- | Get the component id of your component
getComponentId ::
  Sim ComponentId
getComponentId = Sim $ gets currentComponent

-- | Get the node id of of the node your component is currently running on
getNodeId ::
  Sim NodeId
getNodeId = Sim $ gets currentNode

-- | Create a new node
createNode ::
  Sim NodeId -- ^ NodeId of the created node
createNode = Sim $ do
  nodeId <- getUniqueM
  let newNode = Node nodeId NodeInfo Map.empty IM.empty IM.empty []
  modify (\s -> s {nodes = IM.insert nodeId newNode (nodes s)})
  return nodeId

-- | Write memory of local node
writeMemory ::
  Typeable a
  => Int
  -- ^ Address to write
  -> a
  -- ^ Value to write
  -> Sim ()
writeMemory = writeMemoryN Nothing

-- | Write memory of local node
writeMemoryN ::
  Typeable a
  => Maybe NodeId
  -- ^ Node you want to write on, leave 'Nothing' to set to current node
  -> Int
  -- ^ Address to write
  -> a
  -- ^ Value to write
  -> Sim ()
writeMemoryN nodeM addr val = Sim $ do
    node <- fmap (`fromMaybe` nodeM) $ gets currentNode
    lift $ modifyNode node writeVal
  where
    writeVal n@(Node {..}) = n { nodeMemory = IM.insert addr (toDyn val)
                                                nodeMemory }

-- | Read memory of local node
readMemory ::
  Int
  -- ^ Address to read
  -> Sim Dynamic
readMemory = readMemoryN Nothing

-- | Read memory of local node
readMemoryN ::
  Maybe NodeId
  -- ^ Node you want to look on, leave 'Nothing' to set to current node
  -> Int
  -- ^ Address to read
  -> Sim Dynamic
readMemoryN nodeM addr = Sim $ do
  node    <- fmap (`fromMaybe` nodeM) $ gets currentNode
  nodeMem <- fmap (nodeMemory . (IM.! node)) $ gets nodes
  case (IM.lookup addr nodeMem) of
    Just val -> return val
    Nothing  -> error $ "Trying to read empty memory location: " ++
                        show addr ++ " from Node: " ++ show node

-- | Return the 'ComponentId' of the component that created the current
-- component
componentCreator ::
  Sim ComponentId
componentCreator = Sim $ do
  nId <- gets currentNode
  cId <- gets currentComponent
  ns  <- gets nodes
  let ces       = (nodeComponents (ns IM.! nId))
  let ce        = ces IM.! cId
  let ceCreator = creator ce
  return ceCreator

{-# INLINE componentLookup #-}
-- | Get the unique 'ComponentId' of a component implementing an interface
componentLookup ::
  ComponentInterface iface
  => iface
  -- ^ Interface type of the component you are looking for
  -> Sim (Maybe ComponentId)
  -- ^ 'Just' 'ComponentID' if a component is found, 'Nothing' otherwise
componentLookup = componentLookupN Nothing

-- | Get the unique 'ComponentId' of a component implementing an interface
componentLookupN ::
  ComponentInterface iface
  => Maybe NodeId
  -- ^ Node you want to look on, leave 'Nothing' to set to current node
  -> iface
  -- ^ Interface type of the component you are looking for
  -> Sim (Maybe ComponentId)
  -- ^ 'Just' 'ComponentID' if a component is found, 'Nothing' otherwise
componentLookupN nodeM iface = Sim $ do
  node    <- fmap (`fromMaybe` nodeM) $ gets currentNode
  idCache <- fmap (nodeComponentLookup . (IM.! node)) $ lift $ gets nodes
  return $ Map.lookup (componentName iface) idCache

traceMsg ::
  String
  -> Sim ()
traceMsg msg = Sim $ do
  t    <- gets simClk
  node <- gets currentNode
  comp <- gets currentComponent
  lift $ modifyNode node (updateTraceBuffer comp t msg Nothing)

traceMsgTag ::
  String
  -> String
  -> Sim ()
traceMsgTag msg tag = Sim $ do
  t    <- gets simClk
  node <- gets currentNode
  comp <- gets currentComponent
  lift $ modifyNode node (updateTraceBuffer comp t msg (Just tag))

runSTM ::
  STM a
  -> Sim a
runSTM = Sim . lift . lift

getTime ::
  Sim Int
getTime = Sim $ gets simClk

stopSim ::
  Sim ()
stopSim = Sim $ modify (\s -> s {running = False})

newtype HandlerStub = HS ComponentId

instance ComponentInterface HandlerStub where
  type State   HandlerStub = (ComponentId, Dynamic -> Sim ())
  type Receive HandlerStub = Dynamic
  type Send    HandlerStub = ()
  initState _              = undefined
  componentName (HS cId)   = "Asynchronous callback for component " ++
                               show cId
  componentBehaviour _ (waitingFor, handler) (Message _ cnt sender)
    | returnAddress sender == waitingFor
    = Sim $ do
      runSim $ handler cnt
      suspend Kill

  componentBehaviour _ (waitingFor, handler) _  = Sim $ do
    var <- suspend (Request waitingFor return)
    runSim $ handler var
    suspend Kill
