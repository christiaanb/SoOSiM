{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module HeatMap.Application where

import Control.Monad
import Data.Maybe
import qualified Data.IntMap as IM

import SoOSiM
import MemoryManager
import MemoryManager.Types
import Scheduler
import Scheduler.Types

import HeatMap.Types as HeatMap
import HeatMap.Util

heatMapApplication :: HMState -> Input HMMsg -> Sim HMState
-- Initialization behaviour
heatMapApplication hmState (Message Compute retAddr) = do
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
  memManagerId <- fmap fromJust $ componentLookup MemoryManager
  invokeAsync MemoryManager memManagerId
    (Register (MemorySource 0 (2 * w * h) Nothing)) ignore
  mapM_ (\wloc -> invokeAsync MemoryManager memManagerId
                    (Write wloc (0::Float)) ignore
        ) [0..(2*w*h-1)]

  -- Set every 3rd location to 1
  mapM_ (\wloc -> invokeAsync MemoryManager memManagerId
                    (Write wloc (1::Float)) ignore
        ) [ x | x <- [0..(2*w*h-1)], x `mod` 4 == 0]

  -- Instantiate worker threads
  schedulerId <- fmap fromJust $ componentLookup Scheduler
  workerIDs <- mapM (\(wloc,rloc) -> do

                        workerID <- invoke Scheduler schedulerId
                                      (Execute HeatMapWorker
                                        [Register
                                          (MemorySource 0 (2 * w * h)
                                          (Just memManagerId))
                                        ])
                        invokeAsync HeatMapWorker workerID
                          (HeatMap.NewState
                            (HMWorker wloc rloc (transfer hmState)))
                          ignore
                        return workerID
                    ) (zip wlocs rlocs)

  -- Make the worker threads do actual work
  let workers' = IM.fromList (zip workerIDs (repeat Compute))
  mapM_ (\x -> invokeAsync HeatMapWorker x Compute ignore) workerIDs

  yield $ hmState {workers = workers'}

-- Keep track of finished workers
heatMapApplication hmState (Message Done retAddr) = do
  let senderId = returnAddress retAddr
  let workers' = IM.insert senderId Done (workers hmState)
  if (all (== Done) . IM.elems $ workers')
    then do -- All workers are finished
      let (w,h) = arraySize hmState

      -- Calculate read and write locations
      let rlocs = [ dimTrans w h x (0,0) + (w*h) | x <- [0..(w*h)-1]]
      let wlocs = [ dimTrans w h x (0,0) | x <- [0..(w*h)-1]]

      -- locate memory managers
      memManagerId <- fmap fromJust $ componentLookup MemoryManager

      -- Copy values from 1 array to the other
      (rVals :: [Float]) <- mapM (\x -> fmap (unmarshall "rVals") $
                                          invoke MemoryManager
                                            memManagerId (Read x)
                                 ) rlocs
      zipWithM_ (\x v -> invokeAsync MemoryManager memManagerId
                          (Write x v) ignore
                ) wlocs rVals

      traceMsg (show rVals)

      -- Restart all workers to run next iteration
      let workerIDs = IM.keys . workers $ hmState
      mapM_ (\x -> invokeAsync HeatMapWorker x
                     Compute ignore
            ) workerIDs

      yield $ hmState { workers = IM.map (\_ -> Compute) (workers hmState) }
    else do -- Still waiting for some workers
      yield $ hmState { workers = workers' }

heatMapApplication hmState _ = yield hmState

heatMapWorker :: HMWorker -> Input HMMsg -> Sim HMWorker
heatMapWorker hmwState (Message (HeatMap.NewState s') _) =
  yield s'

heatMapWorker hmwState (Message Compute _) = do
  -- Extract configuration
  let (c,vert,hor)   = rdLocs hmwState
  let (dy2i,dx2i,dt) = wtransfer hmwState

  -- Locate memory manager
  memManagerId <- fmap fromJust $ componentLookup MemoryManager

  -- Read array values
  cVal    <- fmap (unmarshall "cval") $ invoke MemoryManager
                                          memManagerId (Read c)
  vertVal <- mapM (\x -> fmap (unmarshall "vertVall") $ invoke MemoryManager
                            memManagerId (Read x)
                   ) vert
  horVal  <- mapM (\x -> fmap (unmarshall "horVal") $ invoke MemoryManager
                            memManagerId (Read x)
                  ) hor

  -- Calculate value
  let newValV = sum ((fromIntegral $ length vert) * cVal:vertVal) * dy2i
  let newValH = sum ((fromIntegral $ length hor) * cVal:horVal) * dx2i
  let newVal = (newValV + newValH) * dt

  -- Write array value
  invokeAsync MemoryManager memManagerId
    (Write (wrLoc hmwState) newVal) ignore

  -- Notify creator that we're finished
  creator <- componentCreator
  invokeAsync HeatMap creator Done ignore

  yield hmwState

heatMapWorker hmwState _ = yield hmwState

data HeatMap       = HeatMap
data HeatMapWorker = HeatMapWorker

-- ComponetIface instances
instance ComponentInterface HeatMap where
  type Receive HeatMap = HMMsg
  type Send HeatMap    = HMMsg
  type State HeatMap   = HMState
  initState _          = HMState IM.empty (2,2) (0.5,0.5,0.5)
  componentName _      = "HeatMap"
  componentBehaviour _ = heatMapApplication

instance ComponentInterface HeatMapWorker where
  type Receive HeatMapWorker = HMMsg
  type Send HeatMapWorker    = HMMsg
  type State HeatMapWorker   = HMWorker
  initState _                = HMWorker 0 (0,[],[]) (0,0,0)
  componentName _            = "HeatMapWorker"
  componentBehaviour _       = heatMapWorker
