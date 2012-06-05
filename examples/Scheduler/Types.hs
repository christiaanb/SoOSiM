{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
module Scheduler.Types where

import SoOSiM
import MemoryManager.Types

data Scheduler = Scheduler

data SchedulerMsg
  = NewState SchedulerState
  | forall iface . (ComponentInterface iface, Typeable (Receive iface)) => Execute iface [MemCommand]
  deriving Typeable

data SchedulerState
  = SchedulerState
  { knownNodes :: [NodeId]
  , usedNodes  :: [NodeId]
  }
