{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module SoOSiM.Simulator where

import           Control.Applicative       ((<$>),(<*>))
import           Control.Concurrent.STM    (TVar,atomically,newTVarIO,readTVar,writeTVar)
import           Control.Concurrent.Supply (freshId,newSupply)
import           Control.Monad.Coroutine   (resume)
import           Control.Monad.State       (execStateT,gets,lift,modify)
import           Data.Dynamic              (Dynamic,Typeable)
import qualified Data.IntMap               as IM
import qualified Data.Map                  as Map
import qualified Data.Traversable          as T

import SoOSiM.Simulator.Util
import SoOSiM.SimMonad
import SoOSiM.Types

tick :: SimState -> IO SimState
tick = atomically . execStateT tick'
  where
    tick' :: SimMonad ()
    tick' = do
      ns <- gets nodes
      _ <- T.mapM executeNode ns
      modify (\s -> s {simClk = simClk s + 1})
      return ()

executeNode ::
  Node
  -> SimMonad ()
executeNode node = do
    modify $ (\s -> s {currentNode = nodeId node})
    _ <- T.mapM executeComponent (nodeComponents node)
    return ()

executeComponent ::
  ComponentContext
  -> SimMonad ()
executeComponent (CC token cId _ statusTV stateTV bufferTV _ metaTV) = do
  modify $ (\s -> s {currentComponent = cId })
  (status,state,buffer) <- lift $ (,,) <$> readTVar statusTV
                                       <*> readTVar stateTV
                                       <*> readTVar bufferTV

  ((status',state'),buffer') <- case (status,buffer) of
    (Killed, _) -> return ((status,state),buffer)
    (Running 0 c, _) -> do
      incrRunningCount metaTV
      r <- handleResult token c state
      return (r,buffer)
    (Running n c, _) -> do
      incrRunningCount metaTV
      return ((Running (n-1) c,state),buffer)
    (ReadyToRun, []) -> do
      incrRunningCount metaTV
      r <- handleResult token (componentBehaviour token state Tick) state
      return (r,[])
    (ReadyToIdle, []) -> do
      incrIdleCount metaTV
      return ((status,state),buffer)
    (WaitingFor _ _, []) -> do
      incrWaitingCount metaTV
      return ((status,state),buffer)
    _ -> do
      t <- gets simClk
      runUntilNothingM (handleInput t) token metaTV status state buffer

  lift $ writeTVar statusTV status' >>
         writeTVar stateTV  state'  >>
         writeTVar bufferTV buffer'

resumeYield ::
  ComponentInterface iface
  => SimInternal (State iface)
  -> SimMonad (ComponentStatus iface, State iface)
resumeYield c = do
  res <- resume c
  case res of
    (Right state') -> return (ReadyToIdle, state')
    (Left _)       -> error "yield did not return state"

handleResult ::
  ComponentInterface iface
  => iface
  -> Sim (State iface)
  -> State iface
  -> SimMonad (ComponentStatus iface, State iface)
handleResult iface f state = do
  res <- resume $ runSim f
  case res of
    Right state'       -> return (ReadyToRun            , state')
    Left (Request o c) -> return (WaitingFor o (Sim . c), state)
    Left (Run i c)     -> return (Running i (Sim c)     , state)
    Left (Yield c)     -> resumeYield c
    Left Kill          -> do
      nId <- gets currentNode
      cId <- gets currentComponent
      modifyNode nId
        (\n -> n { nodeComponents      = IM.delete cId (nodeComponents n)
                 , nodeComponentLookup = Map.delete (componentName iface) (nodeComponentLookup n)
                 }
        )
      return (Killed, state)

runUntilNothingM ::
  Monad m
  => (a -> b -> c -> d -> e -> m ((c,d),Maybe e))
  -> a -> b -> c -> d -> [e]
  -> m ((c,d),[e])
runUntilNothingM _ _     _   st s []         = return ((st, s), [])
runUntilNothingM f iface mTV st s (inp:inps) = do
  (r, inpM) <- f iface mTV st s inp
  case inpM of
    Nothing -> do
      (r',inps') <- runUntilNothingM f iface mTV (fst r) (snd r) inps
      return (r',inps')
    Just _ -> do
      (r',inps') <- runUntilNothingM f iface mTV st s inps
      return (r',inp:inps')

-- | Update component context according to simulator event
handleInput ::
  (ComponentInterface iface, Typeable (Receive iface))
  => Int
  -> iface
  -> TVar SimMetaData
  -> ComponentStatus iface
  -- ^ Current component context
  -> State iface
  -> Input Dynamic
  -- ^ Simulator Event
  -> SimMonad ((ComponentStatus iface, State iface), Maybe (Input Dynamic))
  -- ^ Returns tuple of: ((potentially updated) component context,
  -- (potentially update) component state, 'Nothing' when event is consumed;
  -- 'Just' 'ComponentInput' otherwise)
handleInput t iface metaTV st@(WaitingFor waitingFor f) state
  msg@(Message mTime content sender)
  | waitingFor == (fst $ unRA sender)
  , mTime < t
  = do
    incrRunningCount metaTV
    r <- handleResult iface (f content) state
    return (r,Nothing)
  | otherwise
  = incrWaitingCount metaTV >> return ((st, state), Just msg)

handleInput t iface metaTV st state
  msg@(Message mTime _ _)
  | mTime < t
  = do
    incrRunningCount metaTV
    r <- handleResult iface
          (componentBehaviour iface state (fromDynMsg iface msg))
          state
    return (r,Nothing)
  | otherwise
  = return ((st,state),Just msg)

handleInput _ iface metaTV _ state
  msg@(Tick)
  = do
    incrRunningCount metaTV
    r <- handleResult iface
          (componentBehaviour iface state (fromDynMsg iface msg))
          state
    return (r,Nothing)

initSim :: NodeId -> Sim () -> IO SimState
initSim node0id s = do
  supply <- newSupply
  let (component0id,supply') = freshId supply
      emptyMeta = SimMetaData 0 0 0 Map.empty Map.empty
  statusTV <- newTVarIO ReadyToRun
  stateTV  <- newTVarIO s
  bufferTV <- newTVarIO [Tick]
  emptyMetaTV <- newTVarIO emptyMeta
  let component0CC = CC Initializer component0id component0id statusTV
                        stateTV bufferTV [] emptyMetaTV
      node0 = Node node0id NodeInfo Map.empty
                   (IM.fromList [(component0id,component0CC)])
                   IM.empty [component0id]
      simState = SimState node0id component0id
                          (IM.fromList [(node0id,node0)]) supply' 0 True
  return simState

data Initializer = Initializer

instance ComponentInterface Initializer where
  type Receive Initializer    = ()
  type Send    Initializer    = ()
  type State   Initializer    = Sim ()
  initState                   = const undefined
  componentName _             = "Simulator Initialization"
  componentBehaviour _ s Tick = s >> stop
  componentBehaviour _ s _    = yield s

