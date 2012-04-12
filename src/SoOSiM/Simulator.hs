{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards   #-}
{-# LANGUAGE Rank2Types      #-}
module SoOSiM.Simulator
  ( modifyNode
  , modifyNodeM
  , componentNode
  , updateMsgBuffer
  , updateTraceBuffer
  , execStep
  )
where

import Control.Concurrent.STM
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
modifyNode i f =
  modify (\s -> s {nodes = IM.adjust f (getKey i) (nodes s)})

modifyNodeM ::
  NodeId                   -- ^ ID of the node you want to update
  -> (Node -> SimMonad ()) -- ^ Update function
  -> SimMonad ()
modifyNodeM i f = do
  ns <- gets nodes
  f $ ns IM.! (getKey i)

componentNode ::
  ComponentId
  -> SimMonad NodeId
componentNode cId = do
  let key = getKey cId
  ns <- gets nodes
  let (node:_) = elems $ IM.filter (\n -> IM.member key (nodeComponents n)) ns
  return (nodeId node)

updateMsgBuffer ::
  ComponentId       -- ^ Recipient component ID
  -> ComponentInput -- ^ Actual message
  -> Node           -- ^ Node containing the component
  -> SimMonad ()
updateMsgBuffer recipient msg node = do
    let ce = (nodeComponents node) IM.! (getKey recipient)
    lift $ atomically $ modifyTVar (msgBuffer ce) (\msgs -> msgs ++ [msg])

updateTraceBuffer ::
  ComponentId
  -> String
  -> Node
  -> Node
updateTraceBuffer componentId msg node =
    node { nodeComponents = f (nodeComponents node)}
  where
    f ccs = IM.adjust g (getKey componentId) ccs
    g cc  = cc { traceMsgs = msg:(traceMsgs cc)}

-- | Update component context according to simulator event
handleComponent ::
  ComponentIface s   -- ^ Current component context
  => ComponentStatus s
  -> s
  -> ComponentInput  -- ^ Simulator event
  -> SimMonad (ComponentStatus s, s, Maybe ComponentInput) -- ^ Returns tuple of: ((potentially updated) component context, 'Nothing' when event is consumed; 'Just' 'ComponentInput' otherwise)

-- If a component receives the message from the sender it was waiting for
handleComponent (WaitingForMsg waitingFor f) cstate (ComponentMsg sender content)
  | waitingFor == sender
  = do
    -- Run the resumable computation with the message content
    res <- resume $ runSimM (f content)
    case res of
      -- Computation is finished, return to idle state
      Right a                     -> return (Running, a, Nothing)
      -- Computation is waiting for a message, store the resumable computation
      Left (Request o c) -> return (WaitingForMsg o (SimM . c), cstate, Nothing)
      Left (Yield c)     -> do
        res' <- resume c
        case res' of
          Right a -> return (Idle, a, Nothing)
          Left  _ -> error "yield did not return state!"

-- Don't change the execution context if we're not getting the message we're waiting for
handleComponent st@(WaitingForMsg _ _) s msg
  = return (st, s, Just msg)

-- Not in an waiting state, just handle the message
handleComponent _ cstate msg = do
  res <- resume $ runSimM (componentBehaviour cstate msg)
  case res of
    -- Computation is finished, return to idle state
    Right a            -> return (Running, a, Nothing)
    -- Computation is waiting for a message, store the resumable computation
    Left (Request o c) -> return (WaitingForMsg o (SimM . c), cstate, Nothing)
    Left (Yield c)     -> do
        res' <- resume c
        case res' of
          Right a -> return (Idle, a, Nothing)
          Left  _ -> error "yield did not return state!"

executeComponent ::
  ComponentContext
  -> SimMonad ()
executeComponent (CC cId statusTvar cstateTvar _ bufferTvar _ mDataTV) = do
  modify $ (\s -> s {currentComponent = cId})
  status <- lift $ readTVarIO statusTvar
  cstate <- lift $ readTVarIO cstateTvar
  buffer <- lift $ readTVarIO bufferTvar
  (status',cstate',buffer') <- case (status,buffer) of
        (Running, []) -> do
          res <- resume $ runSimM (componentBehaviour cstate Tick)
          case res of
            Right a            -> return (Running, a, [])
            Left (Request o c) -> return (WaitingForMsg o (SimM . c), cstate, [])
            Left (Yield c)     -> do
                res' <- resume c
                case res' of
                  Right a -> return (Idle, a, [])
                  Left  _ -> error "yield did not return state!"
        _ -> mapUntilNothingM handleComponent status cstate buffer
  lift $ atomically $ writeTVar statusTvar status'
  lift $ atomically $ writeTVar cstateTvar cstate'
  lift $ atomically $ writeTVar bufferTvar buffer'
  lift $ atomically $ modifyCycleCount status buffer mDataTV

modifyCycleCount ::
  ComponentStatus s
  -> [ComponentInput]
  -> TVar SimMetaData
  -> STM ()
modifyCycleCount st bf tv =
  case st of
    Idle ->
      if Prelude.null bf
        then modifyTVar tv (\mdata -> mdata {cyclesIdling  = cyclesIdling  mdata + 1})
        else modifyTVar tv (\mdata -> mdata {cyclesRunning = cyclesRunning  mdata + 1})

    Running           -> modifyTVar tv (\mdata -> mdata {cyclesRunning = cyclesRunning mdata + 1})
    WaitingForMsg _ _ -> modifyTVar tv (\mdata -> mdata {cyclesWaiting = cyclesWaiting mdata + 1})

mapUntilNothingM ::
  ComponentIface s
  => (ComponentStatus s -> s -> ComponentInput -> SimMonad (ComponentStatus s, s, Maybe ComponentInput))
  -> ComponentStatus s
  -> s
  -> [ComponentInput]
  -> SimMonad (ComponentStatus s, s, [ComponentInput])
mapUntilNothingM _ st s [] = return (st,s,[])
mapUntilNothingM f st s (inp:inps) = do
  (st', s', inp_maybe) <- f st s inp
  case inp_maybe of
    Nothing -> return (st',s',inps)
    Just _  -> do
      (st'',s'',inps') <- mapUntilNothingM f st s inps
      return (st'',s'',inp:inps')

executeNode ::
  Node
  -> SimMonad ()
executeNode node = do
  modify $ (\s -> s {currentNode = nodeId node})
  _ <- T.mapM executeComponent (nodeComponents node)
  return ()

tick :: SimMonad ()
tick = do
  ns <- gets nodes
  _ <- T.mapM executeNode ns
  return ()

execStep :: SimState -> IO SimState
execStep = execStateT tick
