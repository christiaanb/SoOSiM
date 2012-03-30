{-# LANGUAGE RecordWildCards #-}
module ResourceDiscovery where

import SoOSiM

import ResourceDiscovery.Types
import ResourceDiscovery.Utils

resourceDiscovery ::
  RDState
  -> ComponentInput
  -> SimM RDState

-- Get a new state, needed for initialization
resourceDiscovery _ (ComponentMsg senderId content)
  | Just (NewState s) <- fromDynamic content
  = return s

-- All nodes behave the same for a discovery request
resourceDiscovery rdState (ComponentMsg senderId content)
  | Just (DiscoveryRequest amount properties) <- fromDynamic content
  = do
    let query       = generateQuery senderId properties
    let qmsId       = localQms rdState
    nodes <- invoke Nothing qmsId (toDyn (Upward amount query))
    invokeNoWait Nothing senderId nodes
    return rdState

-- All nodes behave the same for a lookup request
resourceDiscovery rdState (ComponentMsg senderId content)
  | Just (Lookup amount query) <- fromDynamic content
  = do
    undefined

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
            invokeNoWait Nothing (dhtEntryId rdState) (toDyn (Lookup amount query))
            return rdState
          -- Type C
          else do
            let nextId = nextQms [] (sqmsInstance rdState) (probabilityTable rdState)
            forward msg nextId
            return rdState
      -- Type D
      (False,True,True) -> do
        invokeNoWait Nothing (dhtEntryId rdState) (toDyn (Lookup amount query))
        return rdState

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

-- Aggregate node receiving a Downward Msg
resourceDiscovery rdState@(AggregateState {}) msg@(ComponentMsg senderId content)
  | Just (Downward amount query) <- fromDynamic content
  = do
    case (null $ c2 query) of
      False -> do
        if (match (c2 query) (aggregateStamp rdState))
          -- Type A
          then do
            invokeNoWait Nothing (dhtEntryId rdState) (toDyn (Lookup amount query))
            return rdState
          -- Type B
          else do
            let nextId = nextQms [] (sqmsInstance rdState) (probabilityTable rdState)
            forward msg nextId
            return rdState
      -- Type C
      True -> do
        invokeNoWait Nothing (dhtEntryId rdState) (toDyn (Lookup amount query))
        return rdState

-- Super node receiving an Upward Msg
resourceDiscovery rdState@(SuperState {}) msg@(ComponentMsg senderId content)
  | Just (Upward amount query) <- fromDynamic content
  = do
    case (null $ c3 query) of
      -- Type A
      False -> do
        case (match (c3 query) (superStamp rdState), match (c2 query) (aggregateStamp (aggregateState rdState))) of
          (True,True) -> do
            invokeNoWait Nothing (dhtEntryId $ aggregateState rdState) (toDyn (Lookup amount query))
            return rdState
          (True,False) -> do
            let nextId = nextQms [] (sqmsInstance $ aggregateState rdState) (probabilityTable $ aggregateState rdState)
            invokeNoWait Nothing nextId (toDyn (Downward amount query))
            return rdState
          (False,_) -> do
            undefined
      -- Type B
      True -> do
        aggregateState' <- resourceDiscovery (aggregateState rdState) msg
        return $ rdState {aggregateState = aggregateState'}

-- Super node receiving an Update Msg
resourceDiscovery rdState@(SuperState {}) msg@(ComponentMsg senderId content)
  | Just (Update amount query nodes) <- fromDynamic content
  = do
    if (null nodes)
      then do
        undefined
      else do
        undefined
