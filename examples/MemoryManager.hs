{-# LANGUAGE TypeFamilies #-}
module MemoryManager where

import SoOSiM

import MemoryManager.Types
import MemoryManager.Util

memoryManager :: MemState -> Input MemCommand -> Sim MemState
memoryManager s (Message (Register addr sc src) _)
  = yield $ s {addressLookup = (MemorySource addr sc src):(addressLookup s)}

memoryManager s (Message content@(Read addr) retAddr)
  = do
    let src = checkAddress (addressLookup s) addr
    case (sourceId src) of
      Nothing -> do
        addrVal <- readMemory addr
        respond MemoryManager retAddr addrVal
        yield s
      Just remote -> do
        response <- invoke MemoryManager remote content
        respond MemoryManager retAddr response
        yield s

memoryManager s (Message content@(Write addr val) _)
  = do
    let src = checkAddress (addressLookup s) addr
    case (sourceId src) of
      Nothing -> do
        addrVal <- writeMemory addr val
        yield s
      Just remote -> do
        invokeAsync MemoryManager remote content ignore
        yield s

memoryManager s _ = yield s

instance ComponentInterface MemoryManager where
  type State   MemoryManager = MemState
  type Receive MemoryManager = MemCommand
  type Send    MemoryManager = Dynamic
  initState _                = (MemState [])
  componentName _            = "MemoryManager"
  componentBehaviour _       = memoryManager
