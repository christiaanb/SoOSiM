{-# LANGUAGE DeriveDataTypeable #-}
module HeatMap.Types where

import SoOSiM
import Data.IntMap

data HMMsg = NewState HMWorker | Compute | Done
  deriving (Eq, Typeable)

data HMState = HMState
  { workers   :: IntMap HMMsg
  , arraySize :: (Int,Int)
  , transfer  :: (Float,Float,Float)
  }

data HMWorker = HMWorker
  { wrLoc     :: Int
  , rdLocs    :: (Int,[Int],[Int])
  , wtransfer :: (Float,Float,Float)
  }
  deriving (Eq, Typeable)
