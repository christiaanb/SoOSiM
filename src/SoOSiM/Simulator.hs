{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
module SoOSiM.Simulator where

import           Control.Applicative     ((<$>),(<*>))
import           Control.Concurrent.STM  (TVar,atomically,readTVar,writeTVar)
import           Control.Monad.Coroutine (resume)
import           Control.Monad.State     (execStateT,gets,lift,modify)
import           Data.Dynamic            (Dynamic,Typeable)
import qualified Data.Traversable        as T

import SoOSiM.Simulator.Util
import SoOSiM.Types

tick :: SimState -> IO SimState
tick = atomically . execStateT tick'
  where
    tick' :: SimMonad ()
    tick' = do
      ns <- gets nodes
      _ <- T.mapM executeNode ns
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
    (ReadyToRun, []) -> do
      incrRunningCount metaTV
      r <- handleResult (componentBehaviour token state Tick) state
      return (r,[])
    (ReadyToIdle, []) -> do
      incrIdleCount metaTV
      return ((status,state),buffer)
    (WaitingFor _ _, []) -> do
      incrWaitingCount metaTV
      return ((status,state),buffer)
    _ -> runUntilNothingM handleInput token metaTV status state buffer

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
  => Sim (State iface)
  -> State iface
  -> SimMonad (ComponentStatus iface, State iface)
handleResult f state = do
  res <- resume $ runSim f
  case res of
    Right state'       -> return (ReadyToRun            , state')
    Left (Request o c) -> return (WaitingFor o (Sim . c), state)
    Left (Yield c)     -> resumeYield c

runUntilNothingM ::
  Monad m
  => (a -> b -> c -> d -> e -> m ((c,d),Maybe e))
  -> a -> b -> c -> d -> [e]
  -> m ((c,d),[e])
runUntilNothingM _ _     _   st s []         = return ((st, s), [])
runUntilNothingM f iface mTV st s (inp:inps) = do
  (r, inpM) <- f iface mTV st s inp
  case inpM of
    Nothing -> return (r,inps)
    Just _ -> do
      (r',inps') <- runUntilNothingM f iface mTV st s inps
      return (r',inp:inps')

-- | Update component context according to simulator event
handleInput ::
  (ComponentInterface iface, Typeable (Receive iface))
  => iface
  -> TVar SimMetaData
  -- | Current component context
  -> ComponentStatus iface
  -> State iface
  -- | Simulator Event
  -> Input Dynamic
  -- | Returns tuple of: ((potentially updated) component context,
  -- (potentially update) component state, 'Nothing' when event is consumed;
  -- 'Just' 'ComponentInput' otherwise)
  -> SimMonad ((ComponentStatus iface, State iface), Maybe (Input Dynamic))
handleInput _ metaTV st@(WaitingFor waitingFor f) state
  msg@(Message _ (RA (sender,_)))
  | waitingFor == sender
  = do
    incrRunningCount metaTV
    r <- handleResult (f ()) state
    return (r,Nothing)
  | otherwise
  = incrWaitingCount metaTV >> return ((st, state), Just msg)

handleInput iface metaTV _ state msg = do
  incrRunningCount metaTV
  r <- handleResult
        (componentBehaviour iface state (fromDynMsg iface msg))
        state
  return (r,Nothing)
