{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SoOSiM.SimMonad where

import           Control.Concurrent.STM  (STM,newTVar,readTVar,writeTVar)
import           Control.Monad.Coroutine (suspend)
import           Control.Monad.State     (gets,lift,modify)
import           Data.Dynamic            (Dynamic,Typeable,toDyn)
import qualified Data.IntMap             as IM
import qualified Data.Map                as Map
import           Data.Maybe              (fromMaybe)

import SoOSiM.Simulator.Util
import SoOSiM.Types
import SoOSiM.Util

-- | Create a new component
createComponent ::
  (ComponentInterface iface, Typeable (Receive iface))
  => Maybe NodeId
  -- ^ Node to create component on, leave to 'Nothing' to create on current
  -- node
  -> Maybe ComponentId
  -- ^ ComponentId to set as parent, set to 'Nothing' to use own ComponentId
  -> iface
  -- ^ Component Interface
  -> Sim ComponentId
  -- ^ 'ComponentId' of the created component
createComponent nodeIdM parentIdM iface = Sim $ do
    nodeId    <- fmap (`fromMaybe` nodeIdM) $ gets currentNode
    parentId  <- fmap (`fromMaybe` parentIdM) $ gets currentComponent
    compId    <- getUniqueM

    statusTV  <- (lift . lift) $ newTVar ReadyToRun
    stateTV   <- (lift . lift) $ newTVar (initState iface)
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

-- | Synchronously invoke another component
invoke ::
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
invoke _ senderM recipient content = Sim $ do
  sender       <- fmap (`fromMaybe` senderM) $ gets currentComponent
  responseTV   <- lift . lift . newTVar $ toDyn (undefined :: Send iface)
  let response = RA (sender,responseTV)
  let message  = Message (toDyn content) response

  rNodeId <- lift $ componentNode recipient
  sNodeId <- lift $ componentNode sender
  lift $ modifyNodeM rNodeId (updateMsgBuffer recipient message)
  lift $ modifyNodeM sNodeId (incrSendCounter recipient sender)

  suspend (Request recipient return)
  fmap (unmarshall "invoke") . lift . lift $ readTVar responseTV

-- | Invoke another component, handle response asynchronously
invokeAsync ::
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
  -> (Receive iface)
  -- ^ Argument
  -> (Send iface -> Sim ())
  -- ^ Handler
  -> Sim ()
  -- ^ Call returns immediately
invokeAsync _ senderM recipient content _ = Sim $ do
  sender       <- fmap (`fromMaybe` senderM) $ gets currentComponent
  responseTV   <- lift . lift . newTVar $ toDyn (undefined :: Send iface)
  let response = RA (sender,responseTV)
  let message  = Message (toDyn content) response

  rNodeId <- lift $ componentNode recipient
  sNodeId <- lift $ componentNode sender
  lift $ modifyNodeM rNodeId (updateMsgBuffer recipient message)
  lift $ modifyNodeM sNodeId (incrSendCounter recipient sender)

-- | Respond to another component
respond ::
  forall iface
  . ( ComponentInterface iface
    , Typeable (Send iface))
  => iface
  -- ^ Interface type
  -> Maybe ComponentId
  -- ^ Caller, leave 'Nothing' to set to current module
  -> ReturnAddress
  -- ^ Return address
  -> (Send iface)
  -- ^ Argument
  -> Sim ()
  -- ^ Call returns immediately
respond _ senderM (RA (recipient,respTV)) content = Sim $ do
  sender <- fmap (`fromMaybe` senderM) $ gets currentComponent
  lift . lift $ writeTVar respTV (toDyn content)

  let message = Message undefined (RA (sender,undefined))
  rNodeId <- lift $ componentNode recipient
  sNodeId <- lift $ componentNode sender
  lift $ modifyNodeM rNodeId (updateMsgBuffer recipient message)
  lift $ modifyNodeM sNodeId (incrSendCounter recipient sender)

-- | Yield to the simulator scheduler
yield ::
  a
  -> Sim a
yield s = Sim $ suspend (Yield (return s))

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
  => Maybe NodeId
  -- ^ Node you want to write on, leave 'Nothing' to set to current node
  -> Int
  -- ^ Address to write
  -> a
  -- ^ Value to write
  -> Sim ()
writeMemory nodeM addr val = Sim $ do
    node <- fmap (`fromMaybe` nodeM) $ gets currentNode
    lift $ modifyNode node writeVal
  where
    writeVal n@(Node {..}) = n { nodeMemory = IM.insert addr (toDyn val)
                                                nodeMemory }

-- | Read memory of local node
readMemory ::
  Maybe NodeId
  -- ^ Node you want to look on, leave 'Nothing' to set to current node
  -> Int
  -- ^ Address to read
  -> Sim Dynamic
readMemory nodeM addr = Sim $ do
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

-- | Get the unique 'ComponentId' of a certain component
componentLookup ::
  ComponentInterface iface
  => Maybe NodeId
  -- ^ Node you want to look on, leave 'Nothing' to set to current node
  -> iface
  -- ^ Interface type of the component you are looking for
  -> Sim (Maybe ComponentId)
  -- ^ 'Just' 'ComponentID' if the component is found, 'Nothing' otherwise
componentLookup nodeM iface = Sim $ do
  node    <- fmap (`fromMaybe` nodeM) $ gets currentNode
  idCache <- fmap (nodeComponentLookup . (IM.! node)) $ lift $ gets nodes
  return $ Map.lookup (componentName iface) idCache

traceMsg ::
  String
  -> Sim ()
traceMsg msg = Sim $ do
  node <- gets currentNode
  comp <- gets currentComponent
  lift $ modifyNode node (updateTraceBuffer comp msg)

runSTM ::
  STM a
  -> Sim a
runSTM = Sim . lift . lift
