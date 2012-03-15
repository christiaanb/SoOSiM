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

### Component definition Step-by-Step
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

We now turn to the first line of the actual function definition:

```haskell
memoryManager s (ComponentMsg senderId msgContent) = do
```

Where `memoryManager` is the name of the function, `s` the first argument (of type `MemState`).
We pattern-match on the second argument, meaning this function definition clause only works for values whose constructor is `ComponentMsg`.
By pattern matching we get access to the fields of the datatype, where we bind the names `senderId` and `msgContent` to the values of these fields.

The `do` *keyword* after the `=` sign indicates that the function executes within a monadic environment, the `SimM` environment in our case.
The semantics in a monadic environment are different from those in a normal Haskell functions.
A monadic environment has a more imperative feel, in which your function definition interacts with the environment step-by-step, statement after statements.
This also gives rise to the scoping rules familiar to the imperative programmer: names cannot be used before they are declared.

The next line in our function definition is:

```haskell
  let addrMaybe = identifyAddress msgContent
```

Haskell is whitespace sensitive, so make sure that you have a good editor that does automatic indenting.
We use the `let` constructruct to bind the expression `identifyAddress msgContent` to the name `addrMaybe`.
We use these let-bindings to bind *pure* expressions to names, where *pure* means that the expression has no side-effects [3].
We can now just use the name `addrMaybe` instead of having to type `identifyAddress msgContent` everytime.
Don't worry about efficiency, the evaluation mechanics of Haskell will ensure that the actual expression is only calculated once, even when we use the `addrMaybe` name multiple times.

Because any component could have tried to send an ill-formed message to us, we have defined the utility function `identifyAddress` (in the `MemoryManager.Util` module) to make sure we are actually getting a memory request and subsequently extract the memory address from it.
The value returned by `identifyAddress` is of type `Maybe Int`, which can either be `Just Int` in case the address was found, and `Nothing` in case the message was ill-formed.

Next we define a nested `Case` statement that contains most of the actual behaviour of our memory manager component:

```haskell
  case addrMaybe of
    Just addr ->
      case (addr `elem` localAddress s) of
        True ->
          case (memCommand msgContent) of
            Read _  -> do
              addrVal <- readMemory addr
              sendMessageAsync Nothing senderId addrVal
              return s
            Write _ val -> do
              writeMemory addr (toDyn val)
              sendMessageAsync Nothing senderId (toDyn True)
              return s
```

First we check if an actual memory request was actually send to us, and bind the name `addr` to the value of the address in case it was.
We then check if this address is part of the local address' for which our memory manager is responsible.
For this we use the `elem` function which hase type:

```haskell
elem :: Eq a => a -> [a] -> Bool
```

a function that checks if value is an element of a list, and we surround it by backticks to use it as an infix operator.
We see that we use the field-label `localAddress` of our `MemState` record as a function to extract the value belonging to this field from our internal state `s`.

If we are indeed responsible for this address we check in the third nested case statement if we are either doing a read or a write operation.
In either alternative we must use the `do` keyword again because we will be executing multiple statements.
We will now finally use some of the API functions, the first we encounter is:

```haskell
addrVal <- readMemory addr
```

The `readMemory` function accesses the simulator environment, retreiving the value of the memory location specified by `addr`.
We use the left-arrow `<-` to indicate that this is a side-effecting expression (we are accessing the simulator environment), and that `addrVal` is not bound to the expression itself, but the value of the one-time execution of this statement.

After reading the memory, we send the value back to the module that initially requested the memory access.
We send the value asynchronously because we don't need an actual confirmation value.
Having serviced the request, we use the `return` to give the (unaltered) internal state back to the simulation environment.

In case we our memory manager is not responsible for the address:

```haskell
        False -> do
          creator <- componentCreator
          let remote = findWithDefault creator addr (remoteAddress s)
          response <- sendMessageSync Nothing remote msgContent
          sendMessageAsync Nothing senderId response
          return s
```

We try to find the responsible component for the request from our address-to-component mapping, defaulting to the creator of our module.
We then synchronously send a message to this component, and forward the received response to the component making the original memory request.

In the situations which we didn't handle explicitly, such as receiving something besides a ComponentMsg, or receiving a ComponentMsg which was not a valid memory request, we simply disregard the simulator event, and return our unaltered internal state to the simulator.

#### ComponentIface Instance
At the bottom of our `MemoryManager` module we see the following code:

```haskell
instance ComponentIface MemState where
  initState    = MemState [] empty
  componentFun = memoryManager
```

Here we define a so-called type-class instance.
At this moment you do not need to know what a type-class is, just that you need to define this instance if you want your component to be able to be used by the SoOSiM simulator.

This instance must always contain the definitons for `initState` and `componentFun`, where `initState` is the minimal internal state of your component, and `componentFun` is the function defining the behaviour of your component.
The behaviour of your component must always have the type:

```haskell
s -> ComponentInput -> SimM s
```

Where `s` is the datatype of your component's internal state.

## SoOSiM API

#### Simulator Events
```haskell
data ComponentInput = ComponentMsg ComponentId Dynamic
                    | NodeMsg      NodeId      Dynamic
```

#### Accessing the simulator
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
-- | Create a new component
createComponent ::
  ComponentIface s    -- A ComponentIface instance must be defined for the component state
  => Maybe NodeId     -- ^ Node to create module on, leave to 'Nothing' to create on current node
  -> s                -- ^ Initial state of the component
  -> SimM ComponentId -- ^ ComponentId of the created module
```

#### Handling `Dynamic` Values

```haskell
-- | Converts an arbitrary value into an object of type 'Dynamic'
toDyn :: Typeable a => a -> Dynamic
```

```haskell
-- | Converts a 'Dynamic' object back into an ordinary Haskell value of the correct type.
fromDyn :: 
  Typeable a 
  => Dynamic  -- ^ The dynamically-typed object
  -> a        -- ^ A default value
  -> a        -- ^ Returns: the value of the first argument, if it has the correct type, otherwise the value of the second argument.
```

```haskell
-- | Converts a 'Dynamic' object back into an ordinary Haskell value of the correct type.
fromDynamic :: 
  Typeable a 
  => Dynamic  -- ^ The dynamically-typed object
  -> a        -- ^ A default value
  -> a        -- ^ Returns: 'Just a', if the dynamically-typed object has the correct type (and 'a' is its value), or 'Nothing' otherwise.
```

References
----------

[1] Here is a chapter from a book that introduces the correspondence between Haskell types and C types:
http://book.realworldhaskell.org/read/defining-types-streamlining-functions.html

[2] Some resources that discuss monads: http://book.realworldhaskell.org/read/monads.html and http://learnyouahaskell.com/a-fistful-of-monads

[3] A more elaborate explanation of purity can be found here: http://learnyouahaskell.com/introduction#so-whats-haskell