{-# LANGUAGE RecordWildCards #-}
module SoOSiM.SimMonad where

import Control.Concurrent.STM
import Control.Monad.Coroutine
import Control.Monad.State
import Control.Monad.Trans.Class ()
import Data.IntMap as IntMap
import Data.Map    as Map
import Data.Maybe

import SoOSiM.Simulator
import SoOSiM.Types
import SoOSiM.Util
import Unique
import UniqSupply

-- | Register a component interface with the simulator
registerComponent ::
  ComponentIface s
  => s
  -> SimM ()
registerComponent cstate = SimM $ do
  lift $ modify (\s -> s {componentMap = Map.insert (componentName cstate) (SC cstate) (componentMap s)})

-- | Create a new component
createComponent ::
  Maybe NodeId         -- ^ Node to create component on, leave to 'Nothing' to create on current node
  -> Maybe ComponentId -- ^ ComponentId to set as parent, set to 'Nothing' to use own ComponentId
  -> String            -- ^ Name of the registered component
  -> SimM ComponentId  -- ^ 'ComponentId' of the created component
createComponent nodeId_maybe parentId_maybe cname = SimM $ do
    curNodeId     <- lift $ gets currentNode
    let nId       = fromMaybe curNodeId nodeId_maybe
    pId           <- lift $ gets currentComponent
    let parentId  = fromMaybe pId parentId_maybe
    cId           <- lift getUniqueM

    (SC cstate)   <- fmap (fromJust . Map.lookup cname) $ lift $ gets componentMap
    cstateTV      <- (lift . lift) $ newTVarIO cstate

    statusTV      <- (lift . lift) $ newTVarIO Idle
    bufferTV      <- (lift . lift) $ newTVarIO []

    let emptyMeta = SimMetaData 0 0 0 Map.empty Map.empty
    emptyMetaTV   <- (lift . lift) $ newTVarIO emptyMeta

    lift $ modifyNode nId (addComponent cId (CC cId statusTV cstateTV parentId bufferTV [] emptyMetaTV))
    return cId
  where
    addComponent cId cc n@(Node {..}) =
      n { nodeComponents      = IntMap.insert (getKey cId) cc nodeComponents
        , nodeComponentLookup = Map.insert cname cId nodeComponentLookup
        }

-- | Synchronously invoke another component
invoke ::
  Maybe ComponentId -- ^ Caller, leave 'Nothing' to set to current module
  -> ComponentId    -- ^ Callee
  -> Dynamic        -- ^ Argument
  -> SimM Dynamic   -- ^ Response from recipient
invoke senderMaybe recipient content = SimM $ do
  nId <- lift $ componentNode recipient
  mId <- lift $ gets currentComponent
  let senderId = fromMaybe mId senderMaybe
  senderNodeId <- lift $ componentNode senderId
  lift $ modifyNodeM senderNodeId (incrSendCounter recipient senderId)
  lift $ modifyNodeM nId (updateMsgBuffer recipient (ComponentMsg senderId content))
  suspend (Request recipient return)

-- | Invoke another component, don't wait for a response
invokeNoWait ::
  Maybe ComponentId -- ^ Caller, leave 'Nothing' to set to current module
  -> ComponentId    -- ^ Callee
  -> Dynamic        -- ^ Argument
  -> SimM ()        -- ^ Call returns immediately
invokeNoWait senderMaybe recipient content = SimM $ do
  nId <- lift $ componentNode recipient
  mId <- lift $ gets currentComponent
  let senderId = fromMaybe mId senderMaybe
  senderNodeId <- lift $ componentNode senderId
  lift $ modifyNodeM senderNodeId (incrSendCounter recipient senderId)
  lift $ modifyNodeM nId (updateMsgBuffer recipient (ComponentMsg senderId content))

-- | Yield to the simulator scheduler
yield ::
  ComponentIface s
  => s
  -> SimM s
yield s = SimM $ suspend (Yield (return s))

-- | Get the component id of your component
getComponentId ::
  SimM ComponentId
getComponentId = SimM $ lift $ gets currentComponent

-- | Get the node id of of the node your component is currently running on
getNodeId ::
  SimM NodeId
getNodeId = SimM $ lift $ gets currentNode

-- | Create a new node
createNode ::
  SimM NodeId -- ^ NodeId of the created node
createNode = SimM $ do
  nodeId <- lift getUniqueM
  let newNode = Node nodeId NodeInfo Map.empty IntMap.empty IntMap.empty
  lift $ modify (\s -> s {nodes = IntMap.insert (getKey nodeId) newNode (nodes s)})
  return nodeId

-- | Write memory of local node
writeMemory ::
  Maybe NodeId -- ^ Node you want to write on, leave 'Nothing' to set to current node
  -> Int       -- ^ Address to write
  -> Dynamic   -- ^ Value to write
  -> SimM ()
writeMemory nodeId_maybe i val = SimM $ do
    curNodeId <- lift $ gets currentNode
    let nodeId = fromMaybe curNodeId nodeId_maybe
    lift $ modifyNode nodeId writeVal
  where
    writeVal n@(Node {..}) = n { nodeMemory = IntMap.insert i val nodeMemory }

-- | Read memory of local node
readMemory ::
  Maybe NodeId -- ^ Node you want to look on, leave 'Nothing' to set to current node
  -> Int       -- ^ Address to read
  -> SimM Dynamic
readMemory nodeId_maybe i = SimM $ do
  curNodeId <- lift $ gets currentNode
  let nodeId = getKey $ fromMaybe curNodeId nodeId_maybe
  memVal <- fmap (IntMap.lookup i . nodeMemory . (IntMap.! nodeId)) $ lift $ gets nodes
  case memVal of
    Just val -> return val
    Nothing  -> error $ "Trying to read empty memory location: " ++ show i ++ " from Node: " ++ show (fromMaybe curNodeId nodeId_maybe)

-- | Return the 'ComponentId' of the component that created the current component
componentCreator ::
  SimM ComponentId
componentCreator = SimM $ do
  nId <- fmap getKey $ lift $ gets currentNode
  cId <- fmap getKey $ lift $ gets currentComponent
  ns <- lift $ gets nodes
  let ces       = (nodeComponents (ns IntMap.! nId))
  let ce        = ces IntMap.! cId
  let ceCreator = creator ce
  return ceCreator

-- | Get the unique 'ComponentId' of a certain component
componentLookup ::
  Maybe NodeId                -- ^ Node you want to look on, leave 'Nothing' to set to current node
  -> ComponentName            -- ^ Name of the component you are looking for
  -> SimM (Maybe ComponentId) -- ^ 'Just' 'ComponentID' if the component is found, 'Nothing' otherwise
componentLookup nodeId_maybe cName = SimM $ do
  curNodeId <- lift $ gets currentNode
  let nId   = getKey $ fromMaybe curNodeId nodeId_maybe
  nsLookup  <- fmap (nodeComponentLookup . (IntMap.! nId)) $ lift $ gets nodes
  return $ Map.lookup cName nsLookup

runIO ::
  IO a
  -> SimM a
runIO = SimM . liftIO

traceMsg ::
  String
  -> SimM ()
traceMsg msg = SimM $ do
  curNodeId <- lift $ gets currentNode
  curCompId <- lift $ gets currentComponent
  lift $ modifyNode curNodeId (updateTraceBuffer curCompId msg)
