{-# LANGUAGE DeriveDataTypeable #-}
module ResourceDiscovery where

import qualified Data.List as List
import qualified Data.Map  as Map
import Data.Maybe

import SoOSiM

data RDMsg
  = DiscoveryRequest (Int, [(OP,PropertyAttribute)]) -- Discovery Request send by other components
  | QMSQuery Int Query QMSMsgType
  | FoundNodes [NodeId]
  deriving Typeable

data PropertyAttribute
  = CCR Int
  | PT  PTType
  deriving (Eq,Ord)

data OP
  = Eq -- Equal
  | Ge -- Greater or Equal
  | Le -- Less or Equal

data PTType
  = CPU
  | GPU
  deriving (Eq,Ord)

data Query = Query
  { c1 :: [(OP,PropertyAttribute)]
  , c2 :: [(OP,PropertyAttribute)]
  , c3 :: [(OP,PropertyAttribute)]
  , queryId :: ComponentId
  }

data QMSMsgType
  = LookupMsg
  | UpdwardMsg
  | DownwardMsg
  | UpdateMsg [NodeId]

data QueryType
  = TypeA
  | TypeBC
  | TypeD


exampleRequest = DiscoveryRequest (5,[(Eq,PT CPU), (Ge,CCR 1500)])

exampleQuery = Query
  { c1 = [(Eq,PT CPU)]
  , c2 = [(Ge,CCR 1500)]
  , c3 = []
  , queryId = error "not filled in"
  }

data RDState
  = RDState
  { qmsInstanceId     :: ComponentId
  , sqmsInstanceId    :: ComponentId
  , qmsKeys           :: [PropertyAttribute]
  , qmsDHT            :: DHT
  , probabilityTable  :: ProbabilityTable
  , queryRoutingTable :: Map.Map ComponentId ComponentId
  , queryBlacklist    :: Map.Map ComponentId [ComponentId]
  }

data DHT
  = DHT
  { dhtEntry :: ComponentId
  -- , more
  }

type ProbabilityTable  = [(ComponentId,ProbabilityFactor,Probability)]
type ProbabilityFactor = Float
type Probability       = Float

resourceDiscovery rdState (ComponentMsg senderId content)
  | (Just (DiscoveryRequest (amount,properties))) <- fromDynamic content
  = do
    let query       = generateQuery senderId properties
    let qmsId       = qmsInstanceId rdState
    nodes <- invoke Nothing qmsId (toDyn (QMSQuery amount query UpdwardMsg))
    invokeNoWait Nothing senderId nodes
    return rdState

resourceDiscovery rdState (ComponentMsg senderId content)
  | (Just (QMSQuery amount query UpdwardMsg)) <- fromDynamic content
  = do
    case (queryType query) of
      TypeA  -> do
        let qmsId = qmsInstanceId rdState
        invokeNoWait (Just senderId) qmsId (toDyn (QMSQuery amount query UpdwardMsg))
        return rdState
      TypeBC -> do
        if (match (c2 query) (qmsKeys rdState))
          then do
            nodes <- invoke Nothing (dhtEntry (qmsDHT rdState)) (toDyn (QMSQuery amount query LookupMsg))
            invokeNoWait Nothing senderId nodes
            return rdState
          else do
            let nextId = nextQms [] (sqmsInstanceId rdState) (probabilityTable rdState)
            invokeNoWait (Just senderId) nextId (toDyn (QMSQuery amount query UpdwardMsg))
            return rdState
      TypeD  -> do
        nodes <- invoke Nothing (dhtEntry (qmsDHT rdState)) (toDyn (QMSQuery amount query LookupMsg))
        invokeNoWait Nothing senderId nodes
        return rdState

resourceDiscovery rdState (ComponentMsg senderId content)
  | (Just (QMSQuery amount query (UpdateMsg nodes))) <- fromDynamic content
  = do
    let probabilityTable' = updateTable (null nodes) senderId (probabilityTable rdState)
    if (null nodes)
      then do
        let nextId = nextQms [] (sqmsInstanceId rdState) (probabilityTable rdState)
        invokeNoWait (Just senderId) nextId (toDyn (QMSQuery amount query UpdwardMsg))
        return (rdState {probabilityTable = probabilityTable'})
      else do
        let originalRequester = fromJust $ Map.lookup (queryId query) (queryRoutingTable rdState)
        invokeNoWait Nothing originalRequester (toDyn $ FoundNodes nodes)
        return (rdState {probabilityTable = probabilityTable', queryRoutingTable = Map.delete (queryId query) (queryRoutingTable rdState)})

--resourceDiscovery rdState (ComponentMsg senderId content)
--  | (Just (QMSQuery amount query DownwardMsg)) <- fromDynamic content
--  = do


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
updateTable succesfullQuery cId probTable = error "need to define updateTable"

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

queryType ::
  Query
  -> QueryType
queryType (Query c1 c2 c3 _)
  | null c2 && null c3 = TypeD
  | null c3            = TypeBC
  | not (null c1)      = TypeA
  | otherwise          = error "invalid query"



