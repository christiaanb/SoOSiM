module MemoryManager where

import Data.IntMap
import SoOSiM

import MemoryManager.Types
import MemoryManager.Util

memoryManager :: MemState -> ComponentInput -> SimM MemState
memoryManager s (ComponentMsg senderId msgContent)
  | Just (Register addr sc src) <- safeUnmarshall msgContent
  = yield $ s {addressLookup = (MemorySource addr sc src):(addressLookup s)}

  | Just (Read addr) <- safeUnmarshall msgContent
  = do
    let src = checkAddress (addressLookup s) addr
    case (sourceId src) of
      Nothing -> do
        addrVal <- readMemory Nothing addr
        respond Nothing senderId addrVal
        yield s
      Just remote -> do
        response <- invoke Nothing remote msgContent
        respond Nothing senderId response
        yield s

  | Just (Write addr val) <- safeUnmarshall msgContent
  = do
    let src = checkAddress (addressLookup s) addr
    case (sourceId src) of
      Nothing -> do
        addrVal <- writeMemory Nothing addr val
        yield s
      Just remote -> do
        invokeAsync Nothing remote msgContent ignore
        yield s

memoryManager s _ = return s

instance ComponentIface MemState where
  initState          = MemState []
  componentName _    = "MemoryManager"
  componentBehaviour = memoryManager
