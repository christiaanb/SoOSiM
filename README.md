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

#### MemoryManager.hs
```haskell
module MemoryManager where

import Data.IntMap
import SoOSiM

import MemoryManager.Util

data MemState =
  MemState { localAddress  :: [Int]
           , remoteAddress :: IntMap ComponentId
           }

memoryManager :: MemState -> ComponentInput -> SimM MemState
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


identifyAddress :: Dynamic -> Maybe Int
identifyAddress d = case (fromDynamic d) of
  Just (Write i _) -> Just i
  Just (Read i)    -> Just i
  Nothing          -> Nothing

memCommand :: Dynamic -> MemCommand
memCommand = fromJust . fromDynamic

instance ComponentIface MemState where
  initState    = MemState [] empty
  componentFun = memoryManager
```

#### MemoryManager/Util.hs
```haskell
module MemoryManager.Util where

import Data.Maybe
import SoOSiM

identifyAddress :: Dynamic -> Maybe Int
identifyAddress d = case (fromDynamic d) of
  Just (Write i _) -> Just i
  Just (Read i)    -> Just i
  Nothing          -> Nothing

memCommand :: Dynamic -> MemCommand
memCommand = fromJust . fromDynamic
```

### Module definition Step-by-Step
We will now walk through the code step-by-step:

```haskell
module MemoryManager where
```

We start by defining the name of our Haskell module, in this case `MemoryManager`.
Make sure the name of file matches the name of the module, where haskell src files use the `.hs` file-name extention.

We continue with importing modules that we require to build our component:

```haskell
import Data.IntMap
import SoOSiM

import MemoryManager.Util
```

The `Data.IntMap` module which gives us an efficient datastructure, and corresponding functions, that maps integer keys to values.

### OS Component API
```haskell
-- | Send a message synchronously to another component
sendMessageSync ::
  Maybe ComponentId -- ^ Sender, leave 'Nothing' to set to current module
  -> ComponentId    -- ^ Recipient
  -> Dynamic        -- ^ Message content
  -> SimM Dynamic   -- ^ Response from recipient
```

```haskell
-- | Send a message asynchronously to another component
sendMessageAsync ::
  Maybe ComponentId -- ^ Sender, leave 'Nothing' to set to current module
  -> ComponentId    -- ^ Recipient
  -> Dynamic        -- ^ Message content
  -> SimM ()        -- ^ Call return immediately
```

```haskell
-- | Get the component id of your component
getComponentId ::
  SimM ComponentId
```

```haskell
-- | Get the node id of of the node your component is currently running on
getNodeId ::
  SimM NodeId
```

```haskell
-- | Return the component Id of the component that created the current component
componentCreator ::
  SimM ComponentId
```

```haskell
-- | Write memory of local node
writeMemory ::
  Int        -- ^ Address to write
  -> Dynamic -- ^ Value to write
  -> SimM ()
```

```haskell
-- | Read memory of local node
readMemory ::
  Int -- ^ Address to read
  -> SimM Dynamic
```

```haskell
-- | Create a new node
createNode ::
  Maybe NodeId   -- ^ Connected node, leave 'Nothing' to set to current node
  -> SimM NodeId -- ^ NodeId of the created node
```

```haskell
createComponent ::
  ComponentIface s    -- A ComponentIface instance must be defined for the component state
  => Maybe NodeId     -- ^ Node to create module on, leave to 'Nothing' to create on current node
  -> s                -- ^ Initial state of the component
  -> SimM ComponentId -- ^ ComponentId of the created module
```
