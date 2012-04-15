module ResourceDiscovery.Utils where

import qualified Data.List as List
import SoOSiM

import ResourceDiscovery.Types

localQms ::
	RDState
	-> ComponentId
localQms s@(LeafState {}) 		 = qmsInstance s
localQms s@(AggregateState {}) = qmsInstance $ leafState s
localQms s@(SuperState {})		 = qmsInstance $ leafState $ aggregateState s

generateQuery ::
  ComponentId
  -> [(OP,PropertyAttribute)]
  -> Query
generateQuery cId properties =
    Query {queryId = cId, c1 = c1Properties, c2 = c2Properties, c3 = c3Properties}
  where
    c1Properties = filter (isC1 . snd) properties
    c2Properties = filter (isC2 . snd) properties
    c3Properties = filter (isC3 . snd) properties

isC1 ::
  PropertyAttribute
  -> Bool
isC1 (PT _) = True
isC1 _        = False

isC2 ::
  PropertyAttribute
  -> Bool
isC2 (CCR _) = True
isC2 _         = False

isC3 ::
  PropertyAttribute
  -> Bool
isC3 _ = False

forward ::
	ComponentInput
	-> ComponentId
	-> SimM ()
forward (ComponentMsg senderId content) receiverId = invokeNoWait (Just senderId) receiverId content
forward _ _ = error "forward: not a message"

reply ::
  ComponentId
  -> RDMsg
  -> SimM ()
reply receiverId msg = invokeNoWait Nothing receiverId (toDyn msg)

match ::
  [(OP,PropertyAttribute)]
  -> [PropertyAttribute]
  -> Bool
match [] keys = True
match (condition:conditions) keys =
  (match' condition keys) && match conditions keys

match' ::
  (OP, PropertyAttribute)
  -> [PropertyAttribute]
  -> Bool
match' (Eq,val) keys = any (== val) keys
match' (Le,val) keys = any (<= val) keys
match' (Ge,val) keys = any (>= val) keys

nextQms ::
  [ComponentId]
  -> ComponentId
  -> ProbabilityTable
  -> ComponentId
nextQms blackList sqmsId table
  | null table' = sqmsId
  | otherwise   = cId
  where
    table'    = filter (\(cId,_,_) -> cId `notElem` blackList) table
    (cId,_,_) = List.maximumBy (\(cId1,pf1,prob1) (cId2,pf2,prob2) -> compare prob1 prob2) table'

updateTable ::
  Bool
  -> ComponentId
  -> ProbabilityTable
  -> ProbabilityTable
updateTable succesfullQuery cId probTable = error "no implementation for updateTable"

lookupRouting ::
  ComponentId
  -> RoutingTable
  -> ComponentId
lookupRouting qId routeTable = error "no implementation for lookupRouting"

deleteRoute ::
  ComponentId
  -> RoutingTable
  -> RoutingTable
deleteRoute qId routeTable = error "no implementation for deleteRoute"

--queryType ::
--  Query
--  -> QueryType
--queryType (Query c1 c2 c3 _)
--  | null c2 && null c3 = TypeD
--  | null c3            = TypeBC
--  | not (null c1)      = TypeA
--  | otherwise          = error "invalid query"
