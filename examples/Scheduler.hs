{-# LANGUAGE ScopedTypeVariables #-}
module Scheduler where

import Data.Maybe
import SoOSiM

import Scheduler.Types

scheduler schedState (ComponentMsg sender content) = do
  case (safeUnmarshall content) of
    Just (Execute cname memCommands) -> do
        nodeId <- createNode
        memCompId <- createComponent (Just nodeId) Nothing "MemoryManager"
        mapM_ ((\c -> invokeAsync Nothing memCompId c ignore) . marshall) memCommands
        compId <- createComponent (Just nodeId) (Just sender) cname
        respond Nothing sender (marshall compId)
        return schedState
    Nothing -> return schedState

scheduler schedState _ = return schedState

createComponentRequest ::
  String
  -> SimM ComponentId
createComponentRequest s = do
  schedulerId    <- fmap fromJust $ componentLookup Nothing "Scheduler"
  componentId    <- fmap unmarshall $ invoke Nothing schedulerId (marshall s)
  return componentId

instance ComponentIface SchedulerState where
  initState          = SchedulerState [] []
  componentName _    = "Scheduler"
  componentBehaviour = scheduler
