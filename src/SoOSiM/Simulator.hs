{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards   #-}
module SoOSiM.Simulator
  ( modifyNode
  , updateMsgBuffer
  , executeNode
  )
where

import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Monad.State
import Control.Monad.Trans.Class ()
import Data.IntMap
import Data.Maybe
import qualified Data.Traversable as T

import SoOSiM.Types
import SoOSiM.Util

modifyNode ::
  NodeId            -- ^ ID of the node you want to update
  -> (Node -> Node) -- ^ Update function
  -> SimMonad ()
modifyNode i f = do
  ns <- gets nodes
  modify (\s -> s {nodes = adjust f i ns})

updateMsgBuffer ::
  Int               -- ^ Recipient component ID
  -> ComponentInput -- ^ Actual message
  -> Node           -- ^ Node containing the component
  -> Node
updateMsgBuffer recipient msg n@(Node {..}) = n { nodeComponents = (adjust go recipient nodeComponents) }
  where
    go ce@(CC {..}) = ce {msgBuffer = msg:msgBuffer}

handleComponent ::
  ComponentContext
  -> ComponentInput
  -> SimMonad (ComponentContext, Maybe ComponentInput)
handleComponent (CC status cstate pId buffer) (ComponentMsg sender content)
  | (WaitingForMsg waitingFor f) <- status
  , waitingFor == sender
  = do
    res <- resume $ runSimM (f content)
    case res of
      Right a            -> return (CC Idle a pId buffer, Nothing)
      Left (Request o c) -> return (CC (WaitingForMsg o (SimM . c)) cstate pId buffer, Nothing)

handleComponent ce@(CC status _ _ _) msg
  | (WaitingForMsg _ _) <- status
  = return (ce, Just msg)

handleComponent (CC _ cstate pId buffer) msg = do
  res <- resume $ runSimM (componentBehaviour cstate msg)
  case res of
    Right a            -> return (CC Idle a pId buffer, Nothing)
    Left (Request o c) -> return (CC (WaitingForMsg o (SimM . c)) cstate pId buffer, Nothing)

executeNode ::
  Node
  -> SimMonad Node
executeNode n@(Node nId _ _ components _) = do
  modify $ (\s -> s {currentNode = nId})
  components' <- T.mapM executeComponent components
  return (n {nodeComponents = components'})

executeComponent ::
  ComponentContext
  -> SimMonad ComponentContext
executeComponent ce = do
  (ce',buffer') <- mapAccumLM handleComponent ce (msgBuffer ce)
  return (ce' {msgBuffer = catMaybes buffer'})
