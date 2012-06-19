{-# LANGUAGE TypeFamilies #-}
module Scheduler where

import Data.Maybe
import SoOSiM

import Scheduler.Types
import MemoryManager
import MemoryManager.Types

scheduler ::
  SchedulerState
  -> Input SchedulerMsg
  -> Sim SchedulerState
scheduler schedState (Message (Execute iface memCommands) retAddr) = do
  nodeId    <- createNode
  memCompId <- createComponentN MemoryManager nodeId
  mapM_ (\c -> invokeAsync MemoryManager memCompId c ignore)
    memCommands
  compId    <- createComponentNP nodeId (returnAddress retAddr) iface
  respond Scheduler retAddr compId
  yield schedState

scheduler schedState _ = yield schedState

instance ComponentInterface Scheduler where
  type State Scheduler   = SchedulerState
  type Receive Scheduler = SchedulerMsg
  type Send Scheduler    = ComponentId
  initState              = const (SchedulerState [] [])
  componentName          = const ("Scheduler")
  componentBehaviour     = const scheduler
