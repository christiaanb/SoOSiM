{-# LANGUAGE DeriveDataTypeable #-}
module Scheduler where

import Data.Maybe
import Control.Monad.IfElse

import Code
import Node
import SoOSiM
import CodeDeployer

data SchedulerState = SchedulerState (Maybe AppData)
                                     [ComponentId]

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
   appHndlrIdM     <- componentLookup Nothing "ApplicationHandler"
   resMgrIdM       <- componentLookup Nothing "ResourceManager"
   codeDeployerIdM <- componentLookup Nothing "CodeDeployer"
   let argsM = do appHndlrId     <- appHndlrIdM
                  resMgrId       <- resMgrIdM
                  codeDeployerId <- codeDeployerIdM
                  return (SchedulerDependencyIds appHndlrId resMgrId codeDeployerId)
   maybe (return state) (componentBehaviourWithIds state msg) argsM

componentBehaviourWithIds :: SchedulerState -> ComponentInput -> SchedulerDependencyIds -> SimM SchedulerState
componentBehaviourWithIds state@(SchedulerState appD ids) (ComponentMsg _ content) deps
   | Just (Execute f) <- schedulerAction
   = do rsp <- invoke Nothing (appHndlr deps) (toDyn (AppHndlrMsg f))
        case fromDynamic rsp of
         Nothing -> return state
         Just bs -> let (bName, bCode, resReq) = head bs
                    in startThreads (SchedulerState (Just (AppData bs)) ids) 1 bName bCode resReq deps
     
   | Just (CreateThreads numThreads blockName) <- schedulerAction
   = let block = lookupBlock state blockName
     in case block of
          Nothing      -> return state
          (Just (b,r)) -> startThreads state numThreads blockName b r deps
 where schedulerAction :: Maybe SchedulerMsg
       schedulerAction = fromDynamic content

       lookupBlock :: SchedulerState -> BlockName -> Maybe (BlockCode, ResourceReq)
       lookupBlock (SchedulerState Nothing _)             _   = Nothing
       lookupBlock (SchedulerState (Just (AppData xs)) _) bName | null ls   = Nothing
                                                                | otherwise = Just (head ls)
          where ls = [ (c, r) | (b,c,r) <- xs, b == bName]

componentBehaviourWithIds state _ _ = return state

-- Starts several threads and stores the new Ids in the state
startThreads :: SchedulerState -> Int -> BlockName -> BlockCode -> ResourceReq -> SchedulerDependencyIds -> SimM SchedulerState
startThreads state@(SchedulerState appD ids) numThreads bname b r deps = do
  -- nodeDyn <- sendMessageSync Nothing (resMgr deps) (toDyn (RequestResource (numThreads, r)))
  nodeDyn <- invoke Nothing (resMgr deps) (toDyn (RequestResource numThreads r))
  case fromDynamic nodeDyn of
    (Just nodes) -> do newIds <- mapM (\node -> deployAndCompute state
                                          (bname ++ show (nodeId node)) b node deps)
                                      nodes
                       return (SchedulerState appD (catMaybes newIds ++ ids))
    _            -> return state

deployAndCompute :: SchedulerState
                 -> String
                 -> BlockCode
                 -> NodeDef
                 -> SchedulerDependencyIds
                 -> SimM (Maybe ComponentId)
deployAndCompute state@(SchedulerState appData ids) name block node deps = do
  -- cidDyn <- sendMessageSync Nothing (codeDeployer deps) (toDyn (DeployBlock block node))
  cidDyn <- invoke Nothing (codeDeployer deps) (toDyn (DeployBlock name block node))
  let compIdM = fromDynamic cidDyn
  awhen compIdM $ -- sendMessageAsync Nothing compId (toDyn Compute)
                  \compId -> invokeNoWait Nothing compId (toDyn Compute)
  return compIdM
