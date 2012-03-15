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

#### ./examples/MemoryManager.hs
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


instance ComponentIface MemState where
  initState    = MemState [] empty
  componentFun = memoryManager
```

#### ./examples/MemoryManager/Util.hs
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

The `Data.IntMap` module gives us an efficient datastructure, and corresponding functions, that maps integer keys to values.
The `SoOSiM` module defines all the simulator API functions.
Besides these *external* modules, we also import a *local* module called `MemoryManager.Util`, which we define in the `.MemoryManager/Util.hs` file.

We start our description with a datatype definition describing the internal state of our memory manager component:

```haskell
data MemState =
  MemState { localAddress  :: [Int]
           , remoteAddress :: IntMap ComponentId
           }
```

We define a record datatype [1] that has two fields `localAddress` and `remoteAddress`.
The first field is a dynamically sized list with elements of `Int`, which holds the address' for which our memory manager is responsible.
The second field is an `IntMap` datastructure that maps address' to the *Component ID* of the memory manager that is responsible for those address'.

We now start defining the actual behaviour of our memory manager, starting with its type annotation:

```haskell
memoryManager :: MemState -> ComponentInput -> SimM MemState
```

The type defition tells us that the first argument has the type of our internal component state, and the second argument a value of type `ComponentInput`.
The possible values of this type are enumarated in the *OS Component API* section.
The value of the result is of type `SimM MemState`.
This tells us two things:

* The `memoryManager` function is exuted within the `SimM` monad.
* The actual value that is returned is of type `MemState`.

A *monad* is many wonderfully things [2], way too much to explain here, so for the rest of this README we see it as an execution environment.
Only inside this execution environment will we have access to the SoOSiM API functions.

Although we know the types of the arguments and the result of the function, we don't know their actual meaning.
The SoOSiM simulator will call your component behaviour, passing as the first argument its current internal state.
The second argument is an event that triggered the execution of your component: for example a message send to you by another component.
The result that you must ultimately return is the, potentially updated, internal state of your component.

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

[1] Here is a chapter from a book that introduces the correspondence between Haskell types and C types:
http://book.realworldhaskell.org/read/defining-types-streamlining-functions.html