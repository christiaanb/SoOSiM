{-# LANGUAGE TypeFamilies #-}
module MemoryManager where

import SoOSiM

import MemoryManager.Types
import MemoryManager.Util

memoryManager :: MemState -> Input MemCommand -> Sim MemState
memoryManager s (Message content caller)
  | Register memorySource <- content
  = do
    respond MemoryManager caller undefined
    yield $ s {addressLookup = memorySource:(addressLookup s)}

  | Read addr <- content
  = do
    let src = checkAddress (addressLookup s) addr
    case (sourceId src) of
      Nothing -> do
        addrVal <- readMemory addr
        respond MemoryManager caller addrVal
        yield s
      Just remote -> do
        response <- invoke MemoryManager remote content
        respond MemoryManager caller response
        yield s

  | Write addr val <- content
  = do
    let src = checkAddress (addressLookup s) addr
    case (sourceId src) of
      Nothing -> do
        addrVal <- writeMemory addr val
        respond MemoryManager caller undefined
        yield s
      Just remote -> do
        cId <- getComponentId
        invokeAsync MemoryManager remote content
          (\_ -> respondS MemoryManager (Just cId) caller undefined)
        yield s

memoryManager s _ = yield s

instance ComponentInterface MemoryManager where
  type State   MemoryManager = MemState
  type Receive MemoryManager = MemCommand
  type Send    MemoryManager = Dynamic
  initState _                = (MemState [])
  componentName _            = "MemoryManager"
  componentBehaviour _       = memoryManager
