module SoOSiM.SimMonad where

import Control.Monad.State
import Control.Monad.Trans.Class ()

import SoOSiM.CoroutineT
import SoOSiM.Simulator
import SoOSiM.Types
import SoOSiM.Util

-- | Create a new component
createComponent ::
  -- | aap
  ComponentIface s
  => Maybe NodeId     -- ^ Node to create module on, leave to 'Nothing' to create on current node
  -> s                -- ^ Initial state of the component
  -> SimM ComponentId -- ^ ComponentId of the created module
createComponent = undefined

-- | Send a message synchronously to another component
sendMessageSync ::
  Maybe ComponentId -- ^ Sender, leave 'Nothing' to set to current module
  -> ComponentId    -- ^ Recipient
  -> Dynamic        -- ^ Message content
  -> SimM Dynamic
sendMessageSync Nothing recipient content = do
  nId <- getNodeId
  mId <- getComponentId
  modifyNode nId (updateMsgBuffer recipient (ComponentMsg mId content))
  yield recipient

sendMessageSync _ _ _ = error "sendMessageSync"

sendMessageAsync ::
  Maybe ComponentId -- ^ Sender, leave 'Nothing' to set to current module
  -> ComponentId    -- ^ Recipient
  -> Dynamic        -- ^ Message content
  -> SimM ()
sendMessageAsync = error "sendMessageAsync"

-- | Get the component id of your component
getComponentId ::
  SimM ComponentId
getComponentId = lift $ gets currentComponent

-- | Get the node id of of the node your component is currently running on
getNodeId ::
  SimM NodeId
getNodeId = lift $ gets currentNode

-- | Create a new node
createNode ::
  Maybe NodeId   -- ^ Connected node, leave 'Nothing' to set to current node
  -> SimM NodeId -- ^ NodeId of the created node
createNode = undefined

-- | Write memory of local node
writeMemory ::
  Int -- ^ Address to write
  -> Dynamic -- ^ Value to write
  -> SimM ()
writeMemory = error "writeMemory"

-- | Read memory of local node
readMemory ::
  Int -- ^ Address to read
  -> SimM Dynamic
readMemory = error "readMemory"

-- | Return the component Id of the component that created the current component
componentCreator ::
  SimM ComponentId
componentCreator = do
  nId <- getNodeId
  cId <- getComponentId
  ns <- lift $ gets nodes
  case (nodeComponents (ns!nId)) of
    CC ces -> do
      let ce = ces!cId
      let ceCreator = creator ce
      return ceCreator
