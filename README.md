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
```

#### ./examples/MemoryManager/Types.hs
```haskell
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
module MemoryManager.Types where

import SoOSiM

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

data MemoryManager = MemoryManager
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
Make sure the name of file matches the name of the module, where haskell src files use the `.hs` file-name extension.

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

data MemoryManager = MemoryManager
```

We define two record datatypes [1]; and with three fields (`baseAddress`, `scope`, and `sourceId`) and another with one field (`addressLookup`).
The first record type defines an address range (`baseAddress` and `scope`) and an indication which memory manager is responsible for tht memory range.
The second record type, which has only one field, which defines a dynamically-sized list of `MemorySource` elements.

The third datatype is an algebraic datatype defining the kind of messages that can be send to the memory manager: registering a memory range, reading, and writing.

The fourth datatype is a singleton datatype, which will act as the label/name for the interface defining our memory manager.

We now start defining the actual behaviour of our memory manager, starting with its type annotation:

```haskell
memoryManager :: MemState -> Input MemCommand -> Sim MemState
```

The type definition tells us that the first argument has the type of our internal component state, and the second argument a value of type `Input a`, where the `a` is instantiate to the `MemCommand` datatype.
The possible values of the `Input a` type are enumerated in the *OS Component API* section.
The value of the result is of type `Sim MemState`.
This tells us two things:

* The `memoryManager` function is executed within the `Sim` monad.
* The actual value that is returned is of type `MemState`.

A *monad* is many wonderful things [2], way too much to explain here, so for the rest of this README we see it as an execution environment.
Only inside this execution environment will we have access to the SoOSiM API functions.

Although we know the types of the arguments and the result of the function, we don't know their actual meaning.
The SoOSiM simulator will call your component behaviour, passing as the first argument its current internal state.
The second argument is an event that triggered the execution of your component: for example a message send to you by another component.
The result that you must ultimately return is the, potentially updated, internal state of your component.

We now turn to the first line of the actual function definition:

```haskell
memoryManager s (Message content retAddr) = do
```

Where `memoryManager` is the name of the function, `s` the first argument (of type `MemState`).
We pattern-match on the second argument, meaning this function definition clause only works for values whose constructor is `Message`.
By pattern matching we get access to the fields of the datatype, where we bind the names `content` and `retAddr` to the values of these fields.

The `do` *keyword* after the `=` sign indicates that the function executes within a monadic environment, the `Sim` environment in our case.
The semantics in a monadic environment are different from those in a normal Haskell functions.
A monadic environment has a more imperative feel, in which your function definition interacts with the environment step-by-step, statement after statements.
This also gives rise to the scoping rules familiar to the imperative programmer: names cannot be used before they are declared.

Next we define a nested `case`-statement that contains most of the actual behaviour of our memory manager component:

```haskell
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
```

In the first alternative of our case-statement we handle a `Register` message, by updating our address lookup table with an additional memory source.
We `yield` to the simulator with our updated internal state.

In the second alternative we handle a `Read` request.
The next line in our function definition, which checks which specific memory manager is responsible for the address, is:

```haskell
let src = checkAddress (addressLookup s) addr
```

Haskell is white-space sensitive, so make sure that you have a good editor that does automatic indenting.
We use the `let` construct to bind the expression `checkAddress (addressLookup s) addr` to the name `src`.
We use these let-bindings to bind *pure* expressions to names, where *pure* means that the expression has no side-effects [3].
We can now just use the name `src` instead of having to type `checkAddress (addressLookup s) addr` every time.
Don't worry about efficiency, the evaluation mechanics of Haskell will ensure that the actual expression is only calculated once, even when we use the `src` name multiple times.

In the next case-statement we check if the current or a remote memory manager is responsible for handling the address.
In either alternative we must use the `do` keyword again because we will be executing multiple statements.
We will now finally use some of the API functions, the first we encounter is:

```haskell
addrVal <- readMemory addr
```

The `readMemory` function accesses the simulator environment, retrieving the value of the memory location specified by `addr`.
We use the left-arrow `<-` to indicate that this is a side-effecting expression (we are accessing the simulator environment), and that `addrVal` is not bound to the expression itself, but the value belonging to the execution of this statement.

After reading the memory, we send the value back to the module that initially requested the memory access.
We send the read value as a response to the return address (`retAddr`).
Having serviced the request, we use the `yield` function to give the (unaltered) internal state back to the simulation environment.

If a remote memory manager is responsible for the address:

```haskell
Just remote -> do
  response <- invoke MemoryManager remote content
  respond MemoryManager retAddr response
  yield s
```

We then synchronously invoke the remote memory manager with the original read request, and forward the received response to the component making the original memory request.

The third alternative, handling a write request, is analogous to handling a read request.

In the situations which we didn't handle explicitly, such as receiving a `Tick`, we simply disregard the simulator event, and return our unaltered internal state to the simulator.

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

We use our singleton datatype, `MemoryManager`, as the label/name for our ComponentInterface instance.
All (type-)functions in this interface receive the interface label as their first argument.
For the type-functions (such as `State s`) we must explicitly mention the label, for normal function we can just use the underscore (`_`) as a place holder.

The instance must always contain the definitions for `State`, `Receive`, `Send`, `initState`, `componentName` and `componentBehaviour`.
The `State` indicates the datatype representing the internal state of a module.
The `Receive` indicates the datatype of messages that this component is expecting to receive.
The `Send` indicates the datatype of messages this component will send as responses to invocation.
The `initState` function returns a minimal internal state of your component.
The `componentName` is a function returning the globally unique name of your component.
Finally `componentBehaviour` is a function returning the behaviour of your component.
The behaviour of your component must always have the type:

```haskell
(State iface) -> Input (Receive iface) -> Sim (State iface)
```

Where `State iface` is the datatype of your component's internal state, and `Receive iface` is the datetype of the type of messages the component handles.

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
  = Message a ReturnAddress -- ^ A message send by another component: the
                            --   first field is the message content, the
                            --   second field is the address to send
                            --   responses to
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
-- | Return the component Id of the component that created the current
--   component
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
-- | Converts a 'Dynamic' object back into an ordinary Haskell value of the
--   correct type.
unmarshall ::
  Typeable a
  => Dynamic  -- ^ The dynamically-typed object
  -> a        -- ^ Returns: the value of the first argument, if it has the
              -- correct type, otherwise it gives an error.
```

References
----------

[1] Here is a chapter from a book that introduces the correspondence between Haskell types and C types:
http://book.realworldhaskell.org/read/defining-types-streamlining-functions.html

[2] Some resources that discuss monads: http://book.realworldhaskell.org/read/monads.html and http://learnyouahaskell.com/a-fistful-of-monads

[3] A more elaborate explanation of purity can be found here: http://learnyouahaskell.com/introduction#so-whats-haskell
