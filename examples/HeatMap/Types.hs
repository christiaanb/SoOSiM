{-# LANGUAGE DeriveDataTypeable #-}
module HeatMap.Types where

import SoOSiM
import Data.IntMap

data HMMsg = Compute | Done
  deriving (Eq, Typeable)

data HMState = HMState
  { workers   :: IntMap HMMsg
  , arraySize :: (Int,Int)
  , transfer  :: (Int,Int,Int)
  }

data HMWorker = HMWorker
  { wrLoc     :: Int
  , rdLocs    :: (Int,[Int],[Int])
  , wtransfer :: (Int,Int,Int)
  }
