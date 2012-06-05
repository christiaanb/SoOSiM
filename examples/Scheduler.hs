{-# LANGUAGE TypeFamilies #-}
module Scheduler where

import Data.Maybe
import SoOSiM

import Scheduler.Types
import MemoryManager
import MemoryManager.Types

scheduler ::
  Scheduler
  -> SchedulerState
  -> Input SchedulerMsg
  -> Sim SchedulerState
scheduler _ schedState (Message (Execute iface memCommands) retAddr) = do
  nodeId    <- createNode
  memCompId <- createComponent (Just nodeId) Nothing MemoryManager
  mapM_ (\c -> invokeAsync MemoryManager Nothing memCompId c ignore)
    memCommands
  compId    <- createComponent (Just nodeId) (Just $ returnAddress retAddr) iface
  respond Scheduler Nothing retAddr compId
  yield schedState

scheduler _ schedState _ = yield schedState

instance ComponentInterface Scheduler where
  type State Scheduler   = SchedulerState
  type Receive Scheduler = SchedulerMsg
  type Send Scheduler    = ComponentId
  initState              = const (SchedulerState [] [])
  componentName          = const ("Scheduler")
  componentBehaviour     = scheduler
