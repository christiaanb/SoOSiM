{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module SoOSiM.Simulator where

import           Control.Applicative       ((<$>),(<*>))
import           Control.Arrow             (first,second)
import           Control.Concurrent.STM    (atomically,newTVarIO,readTVar,writeTVar)
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
executeComponent (CC token cId _ statusTV stateTV reqTV respTV _ metaTV) = do
  modify $ (\s -> s {currentComponent = cId })
  (status,state,req,resp) <- lift $ (,,,) <$> readTVar statusTV
                                          <*> readTVar stateTV
                                          <*> readTVar reqTV
                                          <*> readTVar respTV

  ((status',state'),(req',resp')) <- case (status,req,resp) of
    (Killed, _, _) -> return ((status,state),(req,resp))
    (Running 0 c, _, _) -> do
      incrRunningCount metaTV
      r <- handleResult token c state
      return (r,(req,resp))
    (Running n c, _, _) -> do
      incrRunningCount metaTV
      return ((Running (n-1) c,state),(req,resp))
    (ReadyToRun, [], _) -> do
      incrRunningCount metaTV
      r <- handleResult token (componentBehaviour token state Tick) state
      return (r,([],[]))
    (ReadyToIdle, [], _) -> do
      incrIdleCount metaTV
      return ((status,state),([],[]))
    (WaitingFor _ _,_,[]) -> do
      incrWaitingCount metaTV
      return ((status,state),(req,[]))
    _ -> do
      incrRunningCount metaTV
      t <- gets simClk
      runUntilNothingM (handleInput t) token status state (req,resp)

  lift $ writeTVar statusTV status' >>
         writeTVar stateTV  state'  >>
         writeTVar reqTV    req'    >>
         writeTVar respTV   resp'

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
  ( iface
    -> ComponentStatus iface
    -> State iface
    -> Either (Input Dynamic) (Input Dynamic)
    -> SimMonad ( (ComponentStatus iface,State iface)
                , Maybe (Either (Input Dynamic) (Input Dynamic))
                )
  )
  -> iface -> ComponentStatus iface -> State iface
  -> ([Input Dynamic],[Input Dynamic])
  -> SimMonad ( (ComponentStatus iface,State iface)
              , ([Input Dynamic],[Input Dynamic])
              )
runUntilNothingM f iface st s (req,resp) = case (req,resp) of
    (_,inp:inps)  -> do (r,msgM) <- f iface st s (Right inp)
                        handleRep r msgM req inps

    (inp:inps,[]) -> do (r,msgM) <- f iface st s (Left inp)
                        handleRep r msgM inps []
    ([],[])       -> return ((st,s),([],[]))

  where
    handleRep r msgM req' resp' = case msgM of
      Nothing -> runUntilNothingM f iface (fst r) (snd r) (req',resp')
      Just (Left msg) -> fmap ((second . first) (msg:))
                       $ runUntilNothingM f iface st s (req',resp')
      Just (Right msg) -> fmap ((second . second) (msg:))
                        $ runUntilNothingM f iface st s (req',resp')

-- | Update component context according to simulator event
handleInput ::
  (ComponentInterface iface, Typeable (Receive iface))
  => Int
  -> iface
  -> ComponentStatus iface
  -- ^ Current component context
  -> State iface
  -> Either (Input Dynamic) (Input Dynamic)
  -- ^ Simulator Event
  -> SimMonad ((ComponentStatus iface, State iface), Maybe (Either (Input Dynamic) (Input Dynamic)))
  -- ^ Returns tuple of: ((potentially updated) component context,
  -- (potentially update) component state, 'Nothing' when event is consumed;
  -- 'Just' 'ComponentInput' otherwise)
handleInput t iface st@(WaitingFor waitingFor f) state
  msg@(Right (Message (mTime,s) content sender))
  | waitingFor == (fst $ unRA sender)
  , mTime < t
  = do
    r <- handleResult iface (f (content,s)) state
    return (r,Nothing)
  | otherwise
  = return ((st, state), Just msg)

-- Don't process messages while in a running state
handleInput _ _  st@(Running _ _) state msg
  = return ((st,state), Just msg)

handleInput _ _ st state (Right _)
  = return ((st,state), Nothing)

handleInput t iface st state
  msg@(Left msgL@(Message (mTime,_) _ _))
  | mTime < t
  = do
    r <- handleResult iface
          (componentBehaviour iface state (fromDynMsg iface msgL))
          state
    return (r,Nothing)
  | otherwise
  = return ((st,state),Just msg)

handleInput _ iface _ state
  (Left Tick)
  = do
    r <- handleResult iface
          (componentBehaviour iface state (fromDynMsg iface Tick))
          state
    return (r,Nothing)

initSim :: NodeId -> Sim () -> IO SimState
initSim node0id s = do
  supply <- newSupply
  let (component0id,supply') = freshId supply
      emptyMeta = SimMetaData 0 0 0 Map.empty Map.empty
  statusTV <- newTVarIO ReadyToRun
  stateTV  <- newTVarIO s
  reqBufTV <- newTVarIO [Tick]
  respBufTV <- newTVarIO []
  emptyMetaTV <- newTVarIO emptyMeta
  let component0CC = CC Initializer component0id component0id statusTV
                        stateTV reqBufTV respBufTV [] emptyMetaTV
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

