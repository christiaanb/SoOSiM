{-# LANGUAGE ScopedTypeVariables #-}
module Scheduler where

import Data.Maybe
import SoOSiM

import Scheduler.Types

scheduler schedState (ComponentMsg sender content) = do
  case (fromDynamic content) of
    (Just (cname :: String)) -> do
        nodeId <- createNode
        compId <- createComponent (Just nodeId) (Just sender) cname
        invokeNoWait Nothing sender (toDyn compId)
        return schedState
    Nothing -> return schedState

scheduler schedState _ = return schedState

createComponentRequest ::
  String
  -> SimM ComponentId
createComponentRequest s = do
  schedulerId    <- fmap fromJust $ componentLookup Nothing "Scheduler"
  componentIdDyn <- invoke Nothing schedulerId (toDyn s)
  return (fromJust $ fromDynamic componentIdDyn)

instance ComponentIface SchedulerState where
  initState          = SchedulerState
  componentName _    = "Scheduler"
  componentBehaviour = scheduler
