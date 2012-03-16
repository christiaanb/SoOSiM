{-# LANGUAGE RecordWildCards #-}
module SoOSiM.SimMonad where

import Control.Monad.Coroutine.SuspensionFunctors
import Control.Monad.State
import Control.Monad.Trans.Class ()
import Data.IntMap as IntMap
import Data.Map    as Map
import Data.Maybe

import SoOSiM.Simulator
import SoOSiM.Types
import SoOSiM.Util

-- | Create a new component
createComponent ::
  ComponentIface s    -- A ComponentIface instance must be defined for the component state
  => Maybe NodeId     -- ^ Node to create component on, leave to 'Nothing' to create on current node
  -> s                -- ^ Initial state of the component
  -> SimM ComponentId -- ^ ComponentId of the created component
createComponent = error "createComponent"

-- | Send a message synchronously to another component
sendMessageSync ::
  Maybe ComponentId -- ^ Sender, leave 'Nothing' to set to current component
  -> ComponentId    -- ^ Recipient
  -> Dynamic        -- ^ Message content
  -> SimM Dynamic   -- ^ Response from recipient
sendMessageSync senderMaybe recipient content = SimM $ do
  nId <- runSimM $ getNodeId
  mId <- runSimM $ getComponentId
  lift $ modifyNode nId (updateMsgBuffer recipient (ComponentMsg (fromMaybe mId senderMaybe) content))
  request recipient

-- | Send a message asynchronously to another component
sendMessageAsync ::
  Maybe ComponentId -- ^ Sender, leave 'Nothing' to set to current component
  -> ComponentId    -- ^ Recipient
  -> Dynamic        -- ^ Message content
  -> SimM ()        -- ^ Call returns immediately
sendMessageAsync = error "sendMessageAsync"

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
  Maybe NodeId   -- ^ Connected node, leave 'Nothing' to set to current node
  -> SimM NodeId -- ^ NodeId of the created node
createNode = error "createNode"

-- | Write memory of local node
writeMemory ::
  Int        -- ^ Address to write
  -> Dynamic -- ^ Value to write
  -> SimM ()
writeMemory i val = SimM $ do
    curNodeId <- runSimM getNodeId
    lift $ modifyNode curNodeId writeVal
  where
    writeVal n@(Node {..}) = n { nodeMemory = IntMap.insert i val nodeMemory }

-- | Read memory of local node
readMemory ::
  Int -- ^ Address to read
  -> SimM Dynamic
readMemory i = SimM $ do
  curNodeId <- runSimM getNodeId
  memVal <- fmap (IntMap.lookup i . nodeMemory . (IntMap.! curNodeId)) $ lift $ gets nodes
  case memVal of
    Just val -> return val
    Nothing  -> error $ "Trying to read empty memory location: " ++ show i ++ " from Node: " ++ show curNodeId

-- | Return the component Id of the component that created the current component
componentCreator ::
  SimM ComponentId
componentCreator = SimM $ do
  nId <- runSimM $ getNodeId
  cId <- runSimM $ getComponentId
  ns <- lift $ gets nodes
  case (nodeComponents (ns IntMap.! nId)) of
    CC ces -> do
      let ce = ces IntMap.! cId
      let ceCreator = creator ce
      return ceCreator

-- | Get the component lookup table
componentLookup ::
  Maybe NodeId                -- ^ Node you want to look on, leave 'Nothing' to set to current node
  -> ComponentName            -- ^ Name of the component you are looking for
  -> SimM (Maybe ComponentId) -- ^ 'Just ComponentID' if the component is found, 'Nothing' otherwise
componentLookup idMaybe cName = SimM $ do
  curNodeId <- runSimM $ getNodeId
  let nId   = fromMaybe curNodeId idMaybe
  nsLookup  <- fmap (nodeComponentLookup . (IntMap.! nId)) $ lift $ gets nodes
  return $ Map.lookup cName nsLookup
