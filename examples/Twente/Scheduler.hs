{-# LANGUAGE DeriveDataTypeable #-}
module Scheduler where

import Data.Maybe

import Code
import Node
import SoOSiM
import CodeDeployer

data SchedulerState = SchedulerState
  { appDataM   :: (Maybe AppData)
  , runningIds :: [ComponentId]
  }

data AppData     = AppData [(BlockName, BlockCode, ResourceReq)]
 deriving (Typeable)

data ResourceReq = ResourceReq
 deriving (Typeable)

data SchedulerDependencyIds = SchedulerDependencyIds
  { appHndlr     :: ComponentId -- ApplicationHandler
  , resMgr       :: ComponentId -- ResourceManager
  , codeDeployer :: ComponentId -- CodeDeployer
  }

data SchedulerMsg = Execute String
                  | CreateThreads Int BlockName
 deriving (Typeable)

data ResourceMgrMsg = RequestResource Int ResourceReq
 deriving (Typeable)

data AppHndlrMsg = AppHndlrMsg String
 deriving (Typeable)

data AppHndlrRsp = AppHndlr AppData
 deriving (Typeable)

data ComponentMsg = Compute
 deriving (Typeable)

instance ComponentIface SchedulerState where

 initState          = SchedulerState Nothing []

 componentName _    = "Scheduler"

 componentBehaviour state msg = do
   -- Find the components that we need and defer the operation
   appHndlrIdM     <- componentLookup Nothing "ApplicationHandler"
   resMgrIdM       <- componentLookup Nothing "ResourceManager"
   codeDeployerIdM <- componentLookup Nothing "CodeDeployer"
   let argsM = do appHndlrId     <- appHndlrIdM
                  resMgrId       <- resMgrIdM
                  codeDeployerId <- codeDeployerIdM
                  return (SchedulerDependencyIds appHndlrId resMgrId codeDeployerId)
   maybe' argsM
     (return state)
     (componentBehaviourWithIds state msg)

-- | Handles incoming messages
componentBehaviourWithIds :: SchedulerState -> ComponentInput -> SchedulerDependencyIds -> SimM SchedulerState
componentBehaviourWithIds state msg deps
   -- Execute File
   | ComponentMsg _ content <- msg
   , Just (Execute f)       <- fromDynamic msg
   = do -- Obtain list of block names, codes and necessary resources
        rsp <- invoke Nothing (appHndlr deps) (toDyn (AppHndlrMsg f)
        -- If a list has been obtained, execute the first block
        maybe' rsp
          (return state)
          (\bs@(b:_) -> let state' = state {appDataM = Just (AppData bs)}
                        in startThreads state' 1 b deps)
     
   -- Create n threads running the block blockName (if it exists)
   | ComponentMsg _ content <- msg
   , Just (CreateThreads numThreads blockName) <- fromDynamic msg
   , Just (b,r) <- lookupBlock state blockName
   = startThreads state numThreads (blockName, b, r) deps)

   --
   | otherwise = return state

 where lookupBlock :: SchedulerState -> BlockName -> Maybe (BlockCode, ResourceReq)
       lookupBlock st bname = do
         (AppData xs) <- appDataM st
         listToMaybe [ (c, r) | (b,c,r) <- xs, b == bName]

-- Starts several threads and stores the new Ids in the state
startThreads :: SchedulerState -> Int -> (BlockName, BlockCode, ResourceReq) -> SchedulerDependencyIds -> SimM SchedulerState
startThreads state numThreads (bname, b, r) deps = do
  nodeDyn <- invoke Nothing (resMgr deps) (toDyn (RequestResource numThreads r))

  -- If we get a list of nodes, deploy the same code in each one of them
  maybe' (fromDynamic nodeDyn) (return state) $ \nodes -> do
    -- deploy the threads and get the component ids
    newIds <- mapM (\node -> deployAndCompute state
                               (bname ++ show (nodeId node)) b node deps)
                               nodes
    -- update the scheduler state
    return $ state { runningIds = catMaybes newIds ++ runningIds state }

-- | Deploys code in one node and requests that it is computed
deployAndCompute :: SchedulerState
                 -> String
                 -> BlockCode
                 -> NodeDef
                 -> SchedulerDependencyIds
                 -> SimM (Maybe ComponentId)
deployAndCompute state name block node deps = do
  -- Request that the block is deployed and obtain component id
  cidDyn <- invoke Nothing (codeDeployer deps) (toDyn (DeployBlock name block node))
  let compIdM = fromDynamic cidDyn

  -- Send compute to the newly created component
  maybe' compIdM (return ()) $ \compId ->
    invokeNoWait Nothing compId (toDyn Compute)

  return compIdM

maybe' :: Maybe a -> b -> (a -> b) -> b
maybe' m d f = maybe d f m
