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
updateMsgBuffer recipient msg node =
    case (nodeComponents node) of
      CC ces -> node { nodeComponents = CC (adjust go recipient ces) }
  where
    go ce@(CE {..}) = ce {msgBuffer = msg:msgBuffer}

handleComponent ::
  ComponentContext s
  -> ComponentInput
  -> SimMonad (ComponentContext s, Maybe ComponentInput)
handleComponent ce (ComponentMsg sender content)
  | (WaitingForMsg waitingFor f) <- currentStatus ce
  , waitingFor == sender
  = do
    res <- resume $ runSimM (f content)
    case res of
      Right a            -> return (ce {currentStatus = Idle, componentState = a}  , Nothing)
      Left (Request o c) -> return (ce {currentStatus = WaitingForMsg o (SimM . c)}, Nothing)

handleComponent ce msg
  | (WaitingForMsg _ _) <- currentStatus ce
  = return (ce, Just msg)

handleComponent ce msg = do
  res <- resume $ runSimM ((compFun ce) (componentState ce) msg)
  case res of
    Right a            -> return (ce {currentStatus = Idle, componentState = a}  , Nothing)
    Left (Request o c) -> return (ce {currentStatus = WaitingForMsg o (SimM . c)}, Nothing)

executeNode ::
  Node
  -> SimMonad Node
executeNode n@(Node nId _ _ (CC components) _) = do
  modify $ (\s -> s {currentNode = nId})
  components' <- T.mapM executeComponent components
  return (n {nodeComponents = CC components'})

executeComponent ::
  ComponentContext s
  -> SimMonad (ComponentContext s)
executeComponent ce = do
  (ce',buffer') <- mapAccumLM handleComponent ce (msgBuffer ce)
  return (ce' {msgBuffer = catMaybes buffer'})
