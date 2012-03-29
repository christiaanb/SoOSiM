{-# LANGUAGE DeriveDataTypeable #-}
module ResourceDiscovery.Types where

import SoOSiM

data RDState
  = LeafState
    { leafStamp        :: [PropertyAttribute]
    , successorTable   :: SuccessorTable
    , qmsInstance      :: ComponentId
    }
  | AggregateState
    { aggregateStamp   :: [PropertyAttribute]
    , probabilityTable :: ProbabilityTable
    , sqmsInstance     :: ComponentId
    , routingTable     :: RoutingTable
    , leafState        :: RDState
    }
  | SuperState
    { superStamp       :: [PropertyAttribute]
    , neighbourTable   :: NeighbourTable
    , aggregateState   :: RDState
    }

data RDMsg
  = DiscoveryRequest
    { amount     :: Int
    , properties :: [(OP,PropertyAttribute)]
    }
  | Lookup
    { amount     :: Int
    , query      :: Query
    }
  | Upward
    { amount     :: Int
    , query      :: Query
    }
  | Downward
    { amount     :: Int
    , query      :: Query
    }
  | Update
    { amount     :: Int
    , query      :: Query
    , foundNodes :: [NodeId]
    }
  | FoundNodes
    { foundNodes :: [NodeId]
    }
  deriving Typeable

data PropertyAttribute
  = CCR Int
  | PT  PTType
  deriving (Eq,Ord)

data PTType
  = CPU
  | GPU
  deriving (Eq,Ord)

data OP
  = Eq -- Equal
  | Ge -- Greater or Equal
  | Le -- Less or Equal

data Query = Query
  { c1      :: [(OP,PropertyAttribute)]
  , c2      :: [(OP,PropertyAttribute)]
  , c3      :: [(OP,PropertyAttribute)]
  , queryId :: ComponentId
  }

type ProbabilityTable  = [(ComponentId,ProbabilityFactor,Probability)]
type ProbabilityFactor = Float
type Probability       = Float

type SuccessorTable   = ()
type NeighbourTable   = ()
type RoutingTable     = ()

--data QueryType
--  = TypeA
--  | TypeBC
--  | TypeD

--data DHT
--  = DHT
--  { dhtEntry :: ComponentId
--  -- , more
--  }


