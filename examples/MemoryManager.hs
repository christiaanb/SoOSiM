{-# LANGUAGE TypeFamilies #-}
module MemoryManager where

import SoOSiM

import MemoryManager.Types
import MemoryManager.Util

memoryManager :: MemState -> Input MemCommand -> Sim MemState
memoryManager s (Message content retAddr) = do
  case content of
    (Register memorySource) -> do
      yield $ s {addressLookup = memorySource:(addressLookup s)}

    (Read addr) -> do
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

    (Write addr val) -> do
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
