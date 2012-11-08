{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
module SoOSiM.Components.DeploymentManager where

import SoOSiM

import SoOSiM.Components.CodeAdapter
import SoOSiM.Components.MemoryManager.Interface
import SoOSiM.Components.Types
import SoOSiM.Components.Utils

data DeploymentManager = DeploymentManager

data DM_Cmd = Deploy  Code Destination
            | Migrate Code ThreadID Destination

data DM_Msg = DeployBlock BlockName

data DM_State = DM_State

data DMDependencyIds = DMDependencyIds
  { caID     :: ComponentId -- CodeAdaptor
  , mmID     :: ComponentId -- MemoryManager
  }

instance ComponentInterface DeploymentManager where
  type State DeploymentManager   = DM_State
  type Receive DeploymentManager = DM_Cmd
  type Send DeploymentManager    = ()
  initState                      = const (DM_State)
  componentName                  = const ("Deployment Manager")
  componentBehaviour             = const deploymentManager

deploymentManager ::
  DM_State
  -> Input DM_Cmd
  -> Sim DM_State
deploymentManager s msg = do
  -- Find the components that we need and defer the operation
   caIdM  <- componentLookup CodeAdapter
   mmIdM  <- componentLookup MemoryManager
   let argsM = do caId'  <- caIdM
                  mmId'  <- mmIdM
                  return (DMDependencyIds caId' mmId')
   maybe' argsM
     (yield s)
     (deploymentManager' s msg)

deploymentManager' ::
  DM_State
  -> Input DM_Cmd
  -> DMDependencyIds
  -> Sim DM_State
deploymentManager' dmState (Message (Deploy code destination) retAddr) deps = do
  -- 1
  adaptedCode <- invoke CodeAdapter (caID deps) (AdaptCode code (dArch destination))
  -- 2
  let sizeOf_adaptedCode = 1
  reservedMem <- invoke MemoryManager (mmID deps) (ReserveMemory destination sizeOf_adaptedCode)
  invokeAsync MemoryManager (mmID deps) (CreateContextEmpty destination) ignore

  -- 3
  -- reservedMem.writeCode adaptedCode
  yield dmState


deploymentManager' dmState (Message (Migrate code threadID destination) retAddr) deps = do
  -- 1
  MM_TC threadContext_Original <- invoke MemoryManager (mmID deps) (ReadThreadContext threadID)
  -- 2
  invokeAsync MemoryManager (mmID deps) (FreeMemory (threadMem threadID)) ignore
  -- 3
  adaptedCode <- invoke CodeAdapter (caID deps) (AdaptCode code (dArch destination))
  -- 4
  threadContext_Adapted <- invoke CodeAdapter (mmID deps) (AdaptContext threadContext_Original (dArch destination))
  -- 5
  let sizeOf_adaptedCode = 1
  reservedMem <- invoke MemoryManager (mmID deps) (ReserveMemory destination sizeOf_adaptedCode)
  let sizeOf_adaptedCodeContext = 1
  reservedMemContext <- invoke MemoryManager (mmID deps) (CreateContext destination sizeOf_adaptedCodeContext)
  -- 6
  -- reservedMem.writeCode adaptedCode
  -- reservedMemoContext.writeContext theadContext_Adapted

  yield dmState

deploymentManager' dmState _ _ = yield dmState
