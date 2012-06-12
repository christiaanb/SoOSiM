SoOSiM - Abstract Full System Simulator
=======================================

Installation
------------

* Download the latest Haskell Platform from: http://hackage.haskell.org/platform/
* Execute on the command-line: `cabal update`
* Execute on the command-line: `cabal install SoOSiM`

Creating OS Components
----------------------

We jump straight into some code, by showing the description of the *Memory Manager* (http://www.soos-project.eu/wiki/index.php/Application_Cases#Memory_Manager)

#### ./examples/MemoryManager.hs
```haskell
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
```

#### ./examples/MemoryManager/Types.hs
```haskell
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
module MemoryManager.Types where

import SoOSiM

data MemoryManager = MemoryManager

data MemorySource
  = MemorySource
  { baseAddress :: Int
  , scope       :: Int
  , sourceId    :: Maybe ComponentId
  }


data MemState =
  MemState { addressLookup :: [MemorySource]
           }

data MemCommand = Register Int Int (Maybe ComponentId)
                | Read     Int
                | forall a . Typeable a => Write Int a
  deriving Typeable
```

#### ./examples/MemoryManager/Util.hs
```haskell
module MemoryManager.Util where

import MemoryManager.Types

checkAddress ::
  [MemorySource]
  -> Int
  -> MemorySource
checkAddress sources addr = case (filter containsAddr sources) of
    []    -> error ("address unknown: " ++ show addr)
    (x:_) -> x
  where
    containsAddr (MemorySource base sc _) = base <= addr && addr < sc
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
import SoOSiM

import MemoryManager.Types
import MemoryManager.Util
```

The `SoOSiM` module defines all the simulator API functions.
Besides the *external* module, we also import two *local* module called `MemoryManager.Types` and `MemoryManager.Util`, which we define in `./MemoryManager/Types.hs` and `./MemoryManager/Util.hs` respectively.

We start our description with a datatype definition describing the internal state of our memory manager component, and the datatype encoding the messages our memory manager will receive:

```haskell
data MemorySource
  = MemorySource
  { baseAddress :: Int
  , scope       :: Int
  , sourceId    :: Maybe ComponentId
  }

data MemState =
  MemState { addressLookup :: [MemorySource]
           }

data MemCommand = Register MemorySource
                | Read     Int
                | forall a . Typeable a => Write Int a
  deriving Typeable
```

We define two record datatypes [1]; and with three fields (`baseAddress`, `scope`, and `sourceId`) and another with one field (`addressLookup`).
The first record type defines an address range (`baseAddress` and `scope`) and an indication which memory manager is responsisable for tht memory range.
The second record type, which has only one field, which defines a dynamically-sized list of `MemorySource` elements.

We now start defining the actual behaviour of our memory manager, starting with its type annotation:

```haskell
memoryManager :: MemState -> Input MemCommand -> Sim MemState
```

The type defition tells us that the first argument has the type of our internal component state, and the second argument a value of type `ComponentInput`.
The possible values of this type are enumarated in the *OS Component API* section.
The value of the result is of type `Sim MemState`.
This tells us two things:

* The `memoryManager` function is exuted within the `Sim` monad.
* The actual value that is returned is of type `MemState`.

A *monad* is many wonderfull things [2], way too much to explain here, so for the rest of this README we see it as an execution environment.
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

#### ComponentInterface Instance
At the bottom of our `MemoryManager` module we see the following code:

```haskell
instance ComponentInterface MemoryManager where
  type State   MemoryManager = MemState
  type Receive MemoryManager = MemCommand
  type Send    MemoryManager = Dynamic
  initState _                = (MemState [])
  componentName _            = "MemoryManager"
  componentBehaviour _       = memoryManager
```

Here we define a so-called type-class instance.
At this moment you do not need to know what a type-class is, just that you need to define this instance if you want your component to be able to be used by the SoOSiM simulator.

This instance must always contain the definitons for `initState`, `componentName` and `componentBehaviour`, where `initState` is the minimal internal state of your component, `componentName` a function returning the globally unique name of your component, and `componentBehaviour` is the function defining the behaviour of your component.
The behaviour of your component must always have the type:

```haskell
(State iface) -> Input (Receive iface) -> Sim (State iface)
```

Where `State iface` is the datatype of your component's internal state.

## SoOSiM API

#### ComponentInterface Type Class
```haskell
-- | Type class that defines every OS component
class ComponentInterface s where
  -- | Type of messages send by the component
  type Send    s
  -- | Type of messages received by the component
  type Receive s
  -- | Type of internal state of the component
  type State   s
  -- | The minimal internal state of your component
  initState          :: s -> State s
  -- | A function returning the unique global name of your component
  componentName      :: s -> ComponentName
  -- | The function defining the behaviour of your component
  componentBehaviour :: s -> State s -> Input (Receive s) -> Sim (State s)
```

#### Simulator Events
```haskell
data Input a
  = Message a ReturnAddress -- ^ A message send by another component: the first field is the message content, the second field is the address to send responses to
  | Tick                    -- ^ Event send every simulation round
```

#### Accessing the simulator
```haskell
-- | Create a new component
createComponent ::
  (ComponentInterface iface, Typeable (Receive iface))
  => iface
  -- ^ Component Interface
  -> Sim ComponentId
  -- ^ 'ComponentId' of the created component
```

```haskell
-- | Synchronously invoke another component
invoke ::
  (ComponentInterface iface, Typeable (Receive iface), Typeable (Send iface))
  => iface
  -- ^ Interface type
  -> ComponentId
  -- ^ ComponentId of callee
  -> Receive iface
  -- ^ Argument
  -> Sim (Send iface)
  -- ^ Response from callee
```

```haskell
-- | Invoke another component, handle response asynchronously
invokeAsync ::
  (ComponentInterface iface, Typeable (Receive iface), Typeable (Send iface))
  => iface
  -- ^ Interface type
  -> ComponentId
  -- ^ ComponentId of callee
  -> Receive iface
  -- ^ Argument
  -> (Send iface -> Sim ())
  -- ^ Response Handler
  -> Sim ()
  -- ^ Call returns immediately
```

```haskell
-- | Respond to an invocation
respond ::
  (ComponentInterface iface, Typeable (Send iface))
  => iface
  -- ^ Interface type
  -> ReturnAddress
  -- ^ Return address to send response to
  -> (Send iface)
  -- ^ Value to send as response
  -> Sim ()
  -- ^ Call returns immediately
```

```haskell
-- | Yield internal state to the simulator scheduler
yield ::
  a
  -> Sim a
```

```haskell
-- | Get the component id of your component
getComponentId ::
  Sim ComponentId
```

```haskell
-- | Get the node id of of the node your component is currently running on
getNodeId ::
  SimM NodeId
```

```haskell
-- | Create a new node
createNode ::
  Sim NodeId -- ^ NodeId of the created node
```

```haskell
-- | Write memory of local node
writeMemory ::
  Typeable a
  => Int
  -- ^ Address to write
  -> a
  -- ^ Value to write
  -> Sim ()
```

```haskell
-- | Read memory of local node
readMemory ::
  Int
  -- ^ Address to read
  -> Sim Dynamic
```

```haskell
-- | Return the component Id of the component that created the current component
componentCreator ::
  Sim ComponentId
```

```haskell
-- | Get the unique 'ComponentId' of a component implementing an interface
componentLookup ::
  ComponentInterface iface
  => iface
  -- ^ Interface type of the component you are looking for
  -> Sim (Maybe ComponentId)
  -- ^ 'Just' 'ComponentID' if a component is found, 'Nothing' otherwise
```

#### Handling `Dynamic` Values

```haskell
-- | Converts a 'Dynamic' object back into an ordinary Haskell value of the correct type.
unmarshall ::
  Typeable a
  => Dynamic  -- ^ The dynamically-typed object
  -> a        -- ^ Returns: the value of the first argument, if it has the correct type, otherwise it gives an error.
```

References
----------

[1] Here is a chapter from a book that introduces the correspondence between Haskell types and C types:
http://book.realworldhaskell.org/read/defining-types-streamlining-functions.html

[2] Some resources that discuss monads: http://book.realworldhaskell.org/read/monads.html and http://learnyouahaskell.com/a-fistful-of-monads

[3] A more elaborate explanation of purity can be found here: http://learnyouahaskell.com/introduction#so-whats-haskell
