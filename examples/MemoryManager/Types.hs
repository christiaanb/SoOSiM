{-# LANGUAGE DeriveDataTypeable #-}
module MemoryManager.Types where

import SoOSiM

data MemCommand = Read  Int
                | Write Int Dynamic
  deriving Typeable
