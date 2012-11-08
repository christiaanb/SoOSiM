{-# LANGUAGE DeriveDataTypeable #-}
module SoOSiM.Components.MemoryManager.Interface where

import SoOSiM
import SoOSiM.Components.Types

data MemoryManager = MemoryManager

data MM_Cmd = ReserveMemory Destination Memory
            | FreeMemory Memory
            | CreateContextEmpty Destination
            | CreateContext Destination Int
            | ReadThreadContext ThreadID
            | Allocate ProcessId Size ResourceDescription
            | Reserve ProcessId VirtualBaseAddress Size
            | Resolve ProcessId MemAddress
            | ReadMem ProcessId MemAddress Size
            | WriteMem ProcessId MemAddress Size [Dynamic]
  deriving Typeable

data MM_Msg
  = MM_TC ThreadContext
  | MM_AL VirtualBaseAddress
  | MM_RS Bool
  | MM_RL NodeId
  | MM_RD [Dynamic]
  | MM_WR Bool
  deriving Typeable

type VirtualBaseAddress = Int
type MemAddress = Int
type Size = Int
type Host = NodeId
