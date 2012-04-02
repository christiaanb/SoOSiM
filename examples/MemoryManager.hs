module MemoryManager where

import Data.IntMap
import SoOSiM

import MemoryManager.Types
import MemoryManager.Util

memoryManager :: MemState -> ComponentInput -> SimM MemState
memoryManager s (ComponentMsg senderId msgContent)
  | Just (Register addr sc src) <- fromDynamic msgContent
  = return $ s {addressLookup = (MemorySource addr sc src):(addressLookup s)}

  | Just (Read addr) <- fromDynamic msgContent
  = do
    let src = checkAddress (addressLookup s) addr
    case (sourceId src) of
      Nothing -> do
        addrVal <- readMemory Nothing addr
        invokeNoWait Nothing senderId addrVal
        return s
      Just remote -> do
        response <- invoke Nothing remote msgContent
        invokeNoWait Nothing senderId response
        return s

  | Just (Write addr val) <- fromDynamic msgContent
  = do
    let src = checkAddress (addressLookup s) addr
    case (sourceId src) of
      Nothing -> do
        addrVal <- writeMemory Nothing addr val
        return s
      Just remote -> do
        invokeNoWait Nothing remote msgContent
        return s

memoryManager s _ = return s

instance ComponentIface MemState where
  initState          = MemState []
  componentName _    = "MemoryManager"
  componentBehaviour = memoryManager
