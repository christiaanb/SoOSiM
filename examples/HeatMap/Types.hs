{-# LANGUAGE DeriveDataTypeable #-}
module HeatMap.Types where

import Control.Concurrent.STM
import Data.IntMap

import SoOSiM

data HMMsg = NewState HMWorker | Compute | Done
  deriving (Eq, Typeable)

data HMState = HMState
  { workers   :: TVar (IntMap HMMsg)
  , arraySize :: (Int,Int)
  , transfer  :: (Float,Float,Float)
  }

data HMWorker = HMWorker
  { wrLoc     :: Int
  , rdLocs    :: (Int,[Int],[Int])
  , wtransfer :: (Float,Float,Float)
  }
  deriving (Eq, Typeable)
