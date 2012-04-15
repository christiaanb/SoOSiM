{-# LANGUAGE DeriveDataTypeable #-}
module Scheduler.Types where

import SoOSiM
import MemoryManager.Types

data SchedulerMsg
  = NewState SchedulerState
  | Execute String [MemCommand]
  deriving Typeable

data SchedulerState
  = SchedulerState
  { knownNodes :: [NodeId]
  , usedNodes  :: [NodeId]
  }
