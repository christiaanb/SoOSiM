{-# LANGUAGE TypeFamilies       #-}
module SoOSiM.Components.MemoryManager where

import SoOSiM
import SoOSiM.Components.MemoryManager.Interface
import SoOSiM.Components.ProcessManager
import SoOSiM.Components.ResourceDiscovery
import SoOSiM.Components.Types
import SoOSiM.Components.Utils

data MM_State = MM_State
  { pmId         :: ComponentId
  , rdId         :: ComponentId
  , memoryTable  :: [MemoryTableEnty]
  }

data MemoryTableEnty =
  MTE ProcessId VirtualBaseAddress Size [Host]

instance ComponentInterface MemoryManager where
  type State MemoryManager   = MM_State
  type Receive MemoryManager = MM_Cmd
  type Send MemoryManager    = MM_Msg
  initState                  = const (MM_State (-1) (-1) [])
  componentName              = const ("Memory Manager")
  componentBehaviour         = const memoryManager

memoryManager ::
  MM_State
  -> Input MM_Cmd
  -> Sim MM_State
memoryManager mmState (Message (Allocate procId sizeNew requirements) retAddr) = do
  (FoundProc procM) <- invoke ProcessManager (pmId mmState) (ProcLookup procId)
  when (isNothing procM) (error "Process does not exist")

  let procEntries = getEntries (memoryTable mmState) procId
  let vmemAddr = case (reverse procEntries) of
                  [] -> 0
                  ((MTE _ base size _):_) -> base + size

  (Suitable potentialHosts) <- invoke ResourceDiscovery (rdId mmState) (FindResources requirements)
  when (null potentialHosts) (error "No appropriate host found")
  let mte = MTE procId vmemAddr sizeNew [(head potentialHosts)]
  let mmState' = mmState {memoryTable = (memoryTable mmState) ++ [mte]}

  respond MemoryManager retAddr (MM_AL vmemAddr)

  yield mmState'

memoryManager mmState (Message (Reserve procId vmemAddr sizeNew) retAddr) = do
  physicalInstance <- getNodeId
  let mte = MTE procId vmemAddr sizeNew [physicalInstance]
  let mmState' = mmState {memoryTable = (memoryTable mmState) ++ [mte]}
  respond MemoryManager retAddr (MM_RS True)
  yield mmState'

memoryManager mmState (Message (Resolve procId memAddr) retAddr) = do
  host <- resolve (memoryTable mmState) procId memAddr
  respond MemoryManager retAddr (MM_RL host)
  yield mmState

memoryManager mmState (Message (ReadMem procId memAddr sz) retAddr) = do
  let result = []
  mapM_ (resolve (memoryTable mmState) procId) [memAddr .. memAddr+sz]
  respond MemoryManager retAddr (MM_RD result)
  yield mmState

memoryManager mmState (Message (WriteMem procId memAddr sz wrData) retAddr) = do
  mapM_ (resolve (memoryTable mmState) procId) [memAddr .. memAddr+sz]
  respond MemoryManager retAddr (MM_WR True)
  yield mmState

memoryManager s _ = yield s

resolve ::
  [MemoryTableEnty]
  -> ProcessId
  -> MemAddress
  -> Sim NodeId
resolve memTbl procId memAddr = do
  let procEntries = getEntries memTbl procId
  when (null procEntries) (error "Cannot resolve address")
  let mtes = filter (\(MTE _ vmemAddr sz _) ->
                       vmemAddr <= memAddr &&
                       vmemAddr+sz > memAddr)
                    memTbl
  let host = case mtes of
              [] -> error "Cannot resolve address"
              (MTE _ _ _ hosts):_ -> head hosts
  return host

getEntries ::
  [MemoryTableEnty]
  -> ProcessId
  -> [MemoryTableEnty]
getEntries memTbl procId = filter (\(MTE pId _ _ _) -> pId == procId) memTbl
