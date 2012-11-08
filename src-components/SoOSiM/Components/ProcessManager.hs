{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies       #-}
module SoOSiM.Components.ProcessManager where

import SoOSiM
import SoOSiM.Components.ApplicationHandler
import SoOSiM.Components.DeploymentManager
import SoOSiM.Components.ResourceDiscovery
import SoOSiM.Components.Types

data ProcessManager = ProcessManager

type PM_State = PM_State
  { appDataM   = Maybe AppData
  , runningIds = [ProcessId]
  }

data PM_Cmd
  = ProcLookup ProcessId
  | Execute String
  | CreateThreads Int BlockName
  deriving Typeable

data PM_Msg
  = FoundProc (Maybe Process)
  deriving Typeable

data PM_DependencyIds = PMDependencyIds
  { appHndlr :: ComponentId -- ApplicationHandler
  , resMgr   :: ComponentId -- ResourceManager
  , depMgr   :: ComponentId -- DeploymentManager
  , sched    :: ComponentId -- Scheduler
  }

instance ComponentInterface ProcessManager where
  type State ProcessManager   = PM_State
  type Receive ProcessManager = PM_Cmd
  type Send ProcessManager    = PM_Msg
  initState                   = const (PM_State Nothing [])
  componentName               = const ("Process Manager")
  componentBehaviour          = const processManager

processManager ::
  PM_State
  -> Input PM_Cmd
  -> Sim PM_State
processManager pmState inp = do
   appHndlrIdM <- componentLookup ApplicationHandler
   resMngrIdM  <- componentLookup ResourceDiscovery
   depMngrIdM  <- componentLookup DeploymentManager
   schedIdM    <- componentLookup Scheduler
   let argsM = do appHndlrId  <- appHndlrIdM
                  resMngrId   <- resMngrIdM
                  depMngrId   <- depMngrIdM
                  schedId     <- schedIdM
                  return (PMDependencyIds appHndlrId resMngrIdM depMngrIdM schedId)
   maybe' argsM
     (yield s)
     (processManager' s msg)

processManager' ::
  PM_State
  -> Input PM_Cmd
  -> PM_DependencyIds
  -> Sim PM_State
-- Execute File
processManager' pmState (Message (Execute app) retAddr) deps = do
  -- Obtain list of block names, codes and necessary resources
  (AH_AppData appDataM') <- invoke ApplicationHandler (appHndlr deps) (GetAppData app)
  -- If a list has been obtained, execute the first block
  pmState'' <- maybe' appDataM'
                (yield state)
                (\AppData blks -> let pmState' = pmState {appDataM = Just (AppData blks)}
                                  in  startThreads state' 1 (head blks) deps)
  yield pmState''

-- Create n threads running the block blockName (if it exists)
processManager' pmState (Message (CreateThreads nmThreads blkName) retAddr) deps
  | Just (b,r) <- lookupBlock state blockName
  = do
    pmState' <- startThreads pmState nmThreads (blkName, b, r) deps)
    yield pmState'
  where
    lookupBlock :: BlockName -> Maybe (BlockCode, ResourceReq)
    lookupBlock bname = do
         (AppData xs) <- appDataM pmState
         listToMaybe [ (c, r) | (b,c,r) <- xs, b == bName]

-- Starts several threads and stores the new Ids in the state
startThreads ::
  PM_State
  -> Int
  -> (BlockName, BlockCode, ResourceDescription)
  -> PM_DependencyIds
  -> SimM PM_State
startThreads pmState numThreads (bname, b, r) deps = do
  (Suitable nds) <- invoke ResourceDiscovery (resMgr deps) (FindResources numThreads r)

  -- If we find enough nodes, deploy the same code in each one of them
  if (nds == numThreads)
    then do
      -- deploy the threads and get the component ids
      newIds <- mapM (\node -> deployAndCompute pmState
                                (bname ++ (show n)) b node deps)
                                nodes
      -- update the processManager state
      let pmState' = pmState { runningIds = catMaybes newIds ++ runningIds pmState}
      return pmState'
    else return pmState

-- | Deploys code in one node and requests that it is computed
deployAndCompute ::
  PM_State
  -> String
  -> BlockCode
  -> NodeDef
  -> PM_DependencyIds
  -> SimM (Maybe ComponentId)
deployAndCompute state name block node deps = do
  -- Request that the block is deployed
  () <- invoke DeploymentManager (depMgr deps) (Deploy block node)

  -- Call the schedular for the deployed block, ask for the pid
  (Started pIdM) <- invoke Schedular (sched deps) (Start name node)

  return pIdM
