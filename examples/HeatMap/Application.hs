module HeatMap.Application where

import Data.Maybe
import qualified Data.IntMap as IM

import SoOSiM
import MemoryManager.Types
import Scheduler.Types

import HeatMap.Types as HeatMap
import HeatMap.Util

heatMapApplication :: HMState -> ComponentInput -> SimM HMState
-- Initialization behaviour
heatMapApplication hmState Initialize = do
  let (w,h) = arraySize hmState

  -- Calculate read locations for worker threads
  let rlocs = [ ( dimTrans w h x (0,0)
                , filter (>= 0) [dimTrans w h x (0,-1), dimTrans w h x (0,1)]
                , filter (>= 0) [dimTrans w h x (-1,0), dimTrans w h x (1,0)])
              | x <- [0..(w*h)-1]
              ]

  -- Calculate write locations for worker threads
  let wlocs = [ dimTrans w h x (0,0) + (w*h) | x <- [0..(w*h)-1]]

  -- zero-out memory
  memManagerId <- fmap fromJust $ componentLookup Nothing "MemoryManager"
  invokeNoWait Nothing memManagerId (toDyn (Register 0 (2 * w * h) Nothing))
  mapM_ (\wloc -> invokeNoWait Nothing memManagerId (toDyn (Write wloc (toDyn (0::Float))))) [0..(2*w*h-1)]

  -- Set every 3rd location to 1
  mapM_ (\wloc -> invokeNoWait Nothing memManagerId (toDyn (Write wloc (toDyn (1::Float))))) [ x | x <- [0..(2*w*h-1)], x `mod` 4 == 0]

  -- Instantiate worker threads
  registerComponent (initState :: HMWorker)
  schedulerId <- fmap fromJust $ componentLookup Nothing "Scheduler"
  workerIDs <- mapM (\(wloc,rloc) -> do
                        workerIDdyn <- invoke Nothing schedulerId (toDyn $ Execute "HeatMapWorker" [Register 0 (2 * w * h) (Just memManagerId)])
                        let workerID = fromJust $ fromDynamic workerIDdyn
                        invokeNoWait Nothing workerID (toDyn $ HeatMap.NewState (HMWorker wloc rloc (transfer hmState)))
                        return workerID
                    ) (zip wlocs rlocs)

  -- Make the worker threads do actual work
  let workers' = IM.fromList (zip (map getKey workerIDs) (repeat Compute))
  mapM_ (\x -> invokeNoWait Nothing x (toDyn Compute)) workerIDs

  yield $ hmState {workers = workers'}

-- Keep track of finished workers
heatMapApplication hmState (ComponentMsg senderId content) | (Just Done) <- fromDynamic content = do
  let workers' = IM.insert (getKey senderId) Done (workers hmState)
  if (all (== Done) . IM.elems $ workers')
    then do -- All workers are finished
      let (w,h) = arraySize hmState

      -- Calculate read and write locations
      let rlocs = [ dimTrans w h x (0,0) + (w*h) | x <- [0..(w*h)-1]]
      let wlocs = [ dimTrans w h x (0,0) | x <- [0..(w*h)-1]]

      -- locate memory managers
      memManagerId <- fmap fromJust $ componentLookup Nothing "MemoryManager"

      -- Copy values from 1 array to the other
      rVals <- mapM (\x -> invoke Nothing memManagerId (toDyn (Read x))) rlocs
      _ <- mapM (\(x,v) -> invokeNoWait Nothing memManagerId (toDyn (Write x v))) (zip wlocs rVals)

      traceMsg (show (map fromDynamic rVals :: [Maybe Float]))

      -- Restart all workers to run next iteration
      let workerIDs = map mkUniqueGrimily . IM.keys . workers $ hmState
      mapM_ (\x -> invokeNoWait Nothing x (toDyn Compute)) workerIDs

      yield $ hmState { workers = IM.map (\_ -> Compute) (workers hmState) }
    else do -- Still waiting for some workers
      yield $ hmState { workers = workers' }

heatMapApplication hmState _ = return hmState


heatMapWorker hmwState (ComponentMsg _ content) | (Just (HeatMap.NewState s')) <- fromDynamic content = do
  yield s'

heatMapWorker hmwState (ComponentMsg _ content) | (Just Compute) <- fromDynamic content = do
  -- Extract configuration
  let (c,vert,hor)   = rdLocs hmwState
  let (dy2i,dx2i,dt) = wtransfer hmwState

  -- Locate memory manager
  memManagerId <- fmap fromJust $ componentLookup Nothing "MemoryManager"

  -- Read array values
  cVal    <- fmap (fromJust . fromDynamic)       $ invoke Nothing memManagerId (toDyn (Read c))
  vertVal <- fmap (map (fromJust . fromDynamic)) $ mapM (\x -> invoke Nothing memManagerId (toDyn (Read x))) vert
  horVal  <- fmap (map (fromJust . fromDynamic)) $ mapM (\x -> invoke Nothing memManagerId (toDyn (Read x))) hor

  -- Calculate value
  let newValV = sum ((fromIntegral $ length vert) * cVal:vertVal) * dy2i
  let newValH = sum ((fromIntegral $ length hor) * cVal:horVal) * dx2i
  let newVal = (newValV + newValH) * dt

  -- Write array value
  invokeNoWait Nothing memManagerId (toDyn (Write (wrLoc hmwState) (toDyn newVal)))

  -- Notify creator that we're finished
  creator <- componentCreator
  invokeNoWait Nothing creator (toDyn Done)

  yield hmwState

heatMapWorker hmwState _ = return hmwState


-- ComponetIface instances
instance ComponentIface HMState where
  initState          = HMState IM.empty (2,2) (0.5,0.5,0.5)
  componentName _    = "HeatMap"
  componentBehaviour = heatMapApplication

instance ComponentIface HMWorker where
  initState          = HMWorker 0 (0,[],[]) (0,0,0)
  componentName _    = "HeatMapWorker"
  componentBehaviour = heatMapWorker
