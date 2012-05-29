{-# LANGUAGE ScopedTypeVariables #-}
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
heatMapApplication hmState (ComponentMsg senderId content) | (Just Compute) <- safeUnmarshall content = do
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
  invokeAsync Nothing memManagerId
    (marshall (Register 0 (2 * w * h) Nothing))
    ignore
  mapM_ (\wloc -> invokeAsync Nothing memManagerId
                    (marshall (Write wloc (0::Float)))
                    ignore
        ) [0..(2*w*h-1)]

  -- Set every 3rd location to 1
  mapM_ (\wloc -> invokeAsync Nothing memManagerId
                    (marshall (Write wloc (1::Float)))
                    ignore
        ) [ x | x <- [0..(2*w*h-1)], x `mod` 4 == 0]

  -- Instantiate worker threads
  registerComponent (initState :: HMWorker)
  schedulerId <- fmap fromJust $ componentLookup Nothing "Scheduler"
  workerIDs <- mapM (\(wloc,rloc) -> do
                        workerID <- fmap unmarshall
                                  $ invoke Nothing schedulerId
                                      (marshall $
                                        Execute "HeatMapWorker"
                                          [Register 0 (2 * w * h)
                                            (Just memManagerId)
                                          ]
                                      )
                        invokeAsync Nothing workerID
                          (marshall $ HeatMap.NewState
                            (HMWorker wloc rloc (transfer hmState)))
                          ignore
                        return workerID
                    ) (zip wlocs rlocs)

  -- Make the worker threads do actual work
  let workers' = IM.fromList (zip workerIDs (repeat Compute))
  mapM_ (\x -> invokeAsync Nothing x (marshall Compute) ignore) workerIDs

  yield $ hmState {workers = workers'}

-- Keep track of finished workers
heatMapApplication hmState (ComponentMsg senderId content) | (Just Done) <- safeUnmarshall content = do
  let workers' = IM.insert senderId Done (workers hmState)
  if (all (== Done) . IM.elems $ workers')
    then do -- All workers are finished
      let (w,h) = arraySize hmState

      -- Calculate read and write locations
      let rlocs = [ dimTrans w h x (0,0) + (w*h) | x <- [0..(w*h)-1]]
      let wlocs = [ dimTrans w h x (0,0) | x <- [0..(w*h)-1]]

      -- locate memory managers
      memManagerId <- fmap fromJust $ componentLookup Nothing "MemoryManager"

      -- Copy values from 1 array to the other
      (rVals :: [Float]) <- mapM (\x -> fmap unmarshall
                                      $ invoke Nothing memManagerId
                                          (marshall (Read x))
                                 ) rlocs
      _ <- mapM (\(x,v) -> invokeAsync Nothing memManagerId
                              (marshall (Write x v))
                              ignore
                )
                (zip wlocs rVals)

      traceMsg (show rVals)

      -- Restart all workers to run next iteration
      let workerIDs = IM.keys . workers $ hmState
      mapM_ (\x -> invokeAsync Nothing x
                     (marshall Compute)
                     ignore
            ) workerIDs

      yield $ hmState { workers = IM.map (\_ -> Compute) (workers hmState) }
    else do -- Still waiting for some workers
      yield $ hmState { workers = workers' }

heatMapApplication hmState _ = return hmState


heatMapWorker hmwState (ComponentMsg _ content) | (Just (HeatMap.NewState s')) <- safeUnmarshall content = do
  yield s'

heatMapWorker hmwState (ComponentMsg _ content) | (Just Compute) <- safeUnmarshall content = do
  -- Extract configuration
  let (c,vert,hor)   = rdLocs hmwState
  let (dy2i,dx2i,dt) = wtransfer hmwState

  -- Locate memory manager
  memManagerId <- fmap fromJust $ componentLookup Nothing "MemoryManager"

  -- Read array values
  cVal    <- fmap unmarshall $ invoke Nothing memManagerId (marshall (Read c))
  vertVal <- fmap (map unmarshall) $ mapM (\x -> invoke Nothing memManagerId (marshall (Read x))) vert
  horVal  <- fmap (map unmarshall) $ mapM (\x -> invoke Nothing memManagerId (marshall (Read x))) hor

  -- Calculate value
  let newValV = sum ((fromIntegral $ length vert) * cVal:vertVal) * dy2i
  let newValH = sum ((fromIntegral $ length hor) * cVal:horVal) * dx2i
  let newVal = (newValV + newValH) * dt

  -- Write array value
  invokeAsync Nothing memManagerId
    (marshall (Write (wrLoc hmwState) newVal))
    ignore

  -- Notify creator that we're finished
  creator <- componentCreator
  respond Nothing creator (marshall Done)

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
