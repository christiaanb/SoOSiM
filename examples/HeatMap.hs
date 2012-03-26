module HeatMap where

import Data.Maybe
import qualified Data.IntMap as IM

import SoOSiM
import MemoryManager.Types
import Scheduler

import HeatMap.Types
import HeatMap.Util


heatMap :: HMState -> ComponentInput -> SimM HMState
-- Initialization behaviour
heatMap hmState Initialize = do
  let (w,h) = arraySize hmState

  -- Calculate read locations for worker threads
  let rlocs = [ ( dimTrans w h x (0,0)
                , filter (/= (-1)) [dimTrans w h x (0,-1), dimTrans w h x (0,1)]
                , filter (/= (-1)) [dimTrans w h x (-1,0), dimTrans w h x (1,0)])
              | x <- [0..(w*h)-1]
              ]

  -- Calculate write locations for worker threads
  let wlocs = [ dimTrans w h x (0,0) + (w*h) | x <- [0..(w*h)-1]]

  -- Instantiate worker threads
  registerComponent (initState :: HMWorker)
  workerIDs <- mapM (\(w,r) -> do
                        workerID <- createComponentRequest "HeatMapWorker"
                        sendMessageAsync Nothing workerID (toDyn $ NewState (HMWorker w r (transfer hmState)))
                        return workerID
                    ) (zip wlocs rlocs)

  -- Make the worker threads do actual work
  let workers' = IM.fromList (zip (map getKey workerIDs) (repeat Compute))
  mapM_ (\x -> sendMessageAsync Nothing x (toDyn Compute)) workerIDs

  return $ hmState {workers = workers'}

-- Behaviour when all worker threads are finished
heatMap hmState Tick | all (== Done) . IM.elems . workers $ hmState = do
  let (w,h) = arraySize hmState

  -- Calculate read and write locations
  let rlocs = [ dimTrans w h x (0,0) + (w*h) | x <- [0..(w*h)-1]]
  let wlocs = [ dimTrans w h x (0,0) | x <- [0..(w*h)-1]]

  -- locate memory managers
  memManagerId <- fmap fromJust $ componentLookup Nothing "MemoryManager"

  -- Copy values from 1 array to the other
  rVals <- mapM (\x -> sendMessageSync Nothing memManagerId (toDyn (Read x))) rlocs
  _ <- mapM (\(x,v) -> sendMessageSync Nothing memManagerId (toDyn (Write x v))) (zip wlocs rVals)

  -- Restart all workers to run next iteration
  let workerIDs = map mkUniqueGrimily . IM.keys . workers $ hmState
  mapM_ (\x -> sendMessageAsync Nothing x (toDyn Compute)) workerIDs

  return $ hmState { workers = IM.map (\_ -> Compute) (workers hmState) }

-- Keep track of finished workers
heatMap hmState (ComponentMsg senderId content) | (Just Done) <- fromDynamic content = do
  return $ hmState { workers = IM.insert (getKey senderId) Done (workers hmState) }

heatMap hmState _ = return hmState


heatMapWorker hmwState (ComponentMsg _ content) | (Just (NewState s')) <- fromDynamic content = do
  return s'

heatMapWorker hmwState (ComponentMsg _ content) | (Just Compute) <- fromDynamic content = do
  -- Extract configuration
  let (c,vert,hor)   = rdLocs hmwState
  let (dy2i,dx2i,dt) = wtransfer hmwState

  -- Locate memory manager
  memManagerId <- fmap fromJust $ componentLookup Nothing "MemoryManager"

  -- Read array values
  cVal    <- fmap (fromJust . fromDynamic)       $ sendMessageSync Nothing memManagerId (toDyn (Read c))
  vertVal <- fmap (map (fromJust . fromDynamic)) $ mapM (\x -> sendMessageSync Nothing memManagerId (toDyn (Read x))) vert
  horVal  <- fmap (map (fromJust . fromDynamic)) $ mapM (\x -> sendMessageSync Nothing memManagerId (toDyn (Read x))) vert

  -- Calculate value
  let newValV = sum ((length vert) * cVal:vertVal) * dy2i
  let newValH = sum ((length hor) * cVal:horVal) * dx2i
  let newVal = (newValV + newValH) * dt

  -- Write array value
  sendMessageAsync Nothing memManagerId (toDyn (Write (wrLoc hmwState) (toDyn (c + newVal))))

  -- Notify creator that we're finished
  creator <- componentCreator
  sendMessageAsync Nothing creator (toDyn Done)

  return hmwState

heatMapWorker hmwState _ = return hmwState


-- ComponetIface instances
instance ComponentIface HMState where
  initState          = HMState IM.empty (0,0) (0,0,0)
  componentName _    = "HeatMap"
  componentBehaviour = heatMap

instance ComponentIface HMWorker where
  initState          = HMWorker 0 (0,[],[]) (0,0,0)
  componentName _    = "HeatMapWorker"
  componentBehaviour = heatMapWorker
