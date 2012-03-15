SoOSiM - Abstract Full System Simulator
=======================================

Installation
------------

* Download the latest Haskell Platform from: http://hackage.haskell.org/platform/
* Run `cabal update` from the command line
* Either clone the git repository, or download and unpack the zip-file from: http://github.com/christiaanb/SoOSiM
* Change directory to the created directory
* Run `cabal install` from the command line

Creating OS Components
----------------------

We jump straight into some code, by showing the description of the *Memory Manager* (http://www.soos-project.eu/wiki/index.php/Application_Cases#Memory_Manager)

```haskell
module MemoryManager where

import SoOSiM
import SoOSiM.Util

instance ComponentIface MemState where
  initState    = MemState [] empty
  componentFun = memoryManager

data MemState =
  MemState { localAddress  :: [Int]
           , remoteAddress :: IntMap ComponentId
           }

memoryManager s (ComponentMsg senderId msgContent) = do
  let addrMaybe = identifyAddress msgContent
  case addrMaybe of
    Just addr -> do
      case (addr `elem` localAddress s) of
        True  -> do
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
          a <- sendMessageSync Nothing remote msgContent
          sendMessageAsync Nothing senderId a
          return s
    Nothing -> return s

memoryManager s _ = return s
```
