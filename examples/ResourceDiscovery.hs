{-# LANGUAGE RecordWildCards #-}
module ResourceDiscovery where

import SoOSiM

import ResourceDiscovery.Types
import ResourceDiscovery.Utils

resourceDiscovery ::
  RDState
  -> ComponentInput
  -> SimM RDState

-- Any node receiving a discovery request
resourceDiscovery rdState (ComponentMsg senderId content)
  | Just (DiscoveryRequest amount properties) <- fromDynamic content
  = do
    let query       = generateQuery senderId properties
    let qmsId       = localQms rdState
    nodes <- invoke Nothing qmsId (toDyn (Upward amount query))
    invokeNoWait Nothing senderId nodes
    return rdState

-- Aggregate node receiving an Upward Msg
resourceDiscovery rdState@(AggregateState {}) msg@(ComponentMsg senderId content)
  | Just (Upward amount query) <- fromDynamic content
  = do
    case (null $ c1 query, null $ c2 query, null $ c3 query) of
      -- Type A
      (False,False,False) -> do
        forward msg (sqmsInstance rdState)
        return rdState
      (False,False,True) -> do
        if (match (c2 query) (aggregateStamp rdState))
          -- Type B
          then do
            undefined
            --nodes <- invoke Nothing (dhtEntry (qmsDHT rdState)) (toDyn (QMSQuery amount query LookupMsg))
            --invokeNoWait Nothing senderId nodes
            --return rdState
          -- Type C
          else do
            let nextId = nextQms [] (sqmsInstance rdState) (probabilityTable rdState)
            forward msg nextId
            return rdState
      -- Type D
      (False,True,True) -> do
        undefined
        --nodes <- invoke Nothing (dhtEntry (qmsDHT rdState)) (toDyn (QMSQuery amount query LookupMsg))
        --invokeNoWait Nothing senderId nodes
        --return rdState

-- Aggregate node receiving an Update Msg
resourceDiscovery rdState@(AggregateState {}) msg@(ComponentMsg senderId content)
  | Just (Update amount query nodes) <- fromDynamic content
  = do
    let rdState' = rdState {probabilityTable = updateTable (null nodes) senderId (probabilityTable rdState)}
    if (null nodes)
      -- Nodes not found
      then do
        let nextId = nextQms [] (sqmsInstance rdState) (probabilityTable rdState')
        forward msg nextId
        return rdState'
      -- Nodes are found
      else do
        let origin    = lookupRouting (queryId query) (routingTable rdState')
        let rdState'' = rdState' {routingTable = deleteRoute (queryId query) (routingTable rdState')}
        reply origin (FoundNodes nodes)
        return rdState''
