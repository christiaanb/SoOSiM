module MemoryManager where

import Data.IntMap
import SoOSiM

import MemoryManager.Types
import MemoryManager.Util

data MemState =
  MemState { localAddress  :: [Int]
           , remoteAddress :: IntMap ComponentId
           }

memoryManager :: MemState -> ComponentInput -> SimM MemState
memoryManager s (ComponentMsg senderId msgContent) = do
  let addrMaybe = identifyAddress msgContent
  case addrMaybe of
    Just addr ->
      case (addr `elem` localAddress s) of
        True  ->
          case (memCommand msgContent) of
            Read _  -> do
              addrVal <- readMemory addr
              sendMessageAsync Nothing senderId addrVal
              return s
            Write _ val -> do
              writeMemory addr (toDyn val)
              sendMessageAsync Nothing senderId (toDyn True)
              return s
        False -> do
          creator <- componentCreator
          let remote = findWithDefault creator addr (remoteAddress s)
          response <- sendMessageSync Nothing remote msgContent
          sendMessageAsync Nothing senderId response
          return s
    Nothing -> return s

memoryManager s _ = return s

instance ComponentIface MemState where
  initState          = MemState [] empty
  componentName _    = "MemoryManager"
  componentBehaviour = memoryManager
