{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards   #-}
module SoOSiM.Simulator
  ( modifyNode
  , componentNode
  , updateMsgBuffer
  , execStep
  )
where

import Control.Monad.Coroutine
import Control.Monad.State
import Control.Monad.Trans.Class ()
import Data.IntMap as IM
import qualified Data.Traversable as T
import Unique

import SoOSiM.Types

modifyNode ::
  NodeId            -- ^ ID of the node you want to update
  -> (Node -> Node) -- ^ Update function
  -> SimMonad ()
modifyNode i f = do
  ns <- gets nodes
  modify (\s -> s {nodes = adjust f (getKey i) ns})

componentNode ::
  ComponentId
  -> SimMonad NodeId
componentNode cId = do
  let key = getKey cId
  ns <- gets nodes
  let [Node nId _ _ _ _] = elems $ IM.filter (\n -> IM.member key (nodeComponents n)) ns
  return nId

updateMsgBuffer ::
  ComponentId       -- ^ Recipient component ID
  -> ComponentInput -- ^ Actual message
  -> Node           -- ^ Node containing the component
  -> Node           -- ^ Updated node
updateMsgBuffer recipient msg n@(Node {..}) = n { nodeComponents = (adjust go (getKey recipient) nodeComponents) }
  where
    go ce@(CC {..}) = ce {msgBuffer = msg:msgBuffer}

-- | Update component context according to simulator event
handleComponent ::
  ComponentContext   -- ^ Current component context
  -> ComponentInput  -- ^ Simulator event
  -> SimMonad (ComponentContext, Maybe ComponentInput) -- ^ Returns tuple of: ((potentially updated) component context, 'Nothing' when event is consumed; 'Just' 'ComponentInput' otherwise)

-- If a component receives the message from the sender it was waiting for
handleComponent (CC (WaitingForMsg waitingFor f) cstate pId buffer) (ComponentMsg sender content)
  | waitingFor == sender
  = do
    -- Run the resumable computation with the message content
    res <- resume $ runSimM (f content)
    case res of
      -- Computation is finished, return to idle state
      Right a                     -> return (CC Running a pId buffer, Nothing)
      -- Computation is waiting for a message, store the resumable computation
      Left (Request o c) -> return (CC (WaitingForMsg o (SimM . c)) cstate pId buffer, Nothing)
      Left (Yield c)     -> do
        res' <- resume c
        case res' of
          Right a -> return (CC Idle a pId buffer, Nothing)
          Left  _ -> error "yield did not return state!"

-- Don't change the execution context if we're not getting the message we're waiting for
handleComponent ce@(CC (WaitingForMsg _ _) _ _ _) msg
  = return (ce, Just msg)

-- Not in an waiting state, just handle the message
handleComponent (CC _ cstate pId buffer) msg = do
  res <- resume $ runSimM (componentBehaviour cstate msg)
  case res of
    -- Computation is finished, return to idle state
    Right a            -> return (CC Running a pId buffer, Nothing)
    -- Computation is waiting for a message, store the resumable computation
    Left (Request o c) -> return (CC (WaitingForMsg o (SimM . c)) cstate pId buffer, Nothing)
    Left (Yield c)     -> do
        res' <- resume c
        case res' of
          Right a -> return (CC Idle a pId buffer, Nothing)
          Left  _ -> error "yield did not return state!"

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
executeComponent (CC Running cstate pId []) = do
  res <- resume $ runSimM (componentBehaviour cstate Tick)
  case res of
    Right a            -> return (CC Running a pId [])
    Left (Request o c) -> return (CC (WaitingForMsg o (SimM . c)) cstate pId [])
    Left (Yield c)     -> do
        res' <- resume c
        case res' of
          Right a -> return (CC Idle a pId [])
          Left  _ -> error "yield did not return state!"

executeComponent ce = do
  (ce',buffer') <- mapUntilNothingM handleComponent ce (msgBuffer ce)
  return (ce' {msgBuffer = buffer'})

mapUntilNothingM ::
  (ComponentContext -> ComponentInput -> SimMonad (ComponentContext, Maybe ComponentInput))
  -> ComponentContext
  -> [ComponentInput]
  -> SimMonad (ComponentContext, [ComponentInput])
mapUntilNothingM _ ce [] = return (ce,[])
mapUntilNothingM f ce (inp:inps) = do
  (ce',inp_maybe) <- f ce inp
  case inp_maybe of
    Nothing -> return (ce',inps)
    Just _  -> do
      (ce'',inps') <- mapUntilNothingM f ce inps
      return (ce'',inp:inps')

tick :: SimMonad ()
tick = do
  ns <- gets nodes
  ns' <- T.mapM executeNode ns
  modify (\s -> s {nodes = ns'})

execStep :: SimState -> IO SimState
execStep = execStateT tick
