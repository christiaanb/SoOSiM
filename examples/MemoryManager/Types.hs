{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
module MemoryManager.Types where

import SoOSiM

data MemorySource
  = MemorySource
  { baseAddress :: Int
  , scope       :: Int
  , sourceId    :: Maybe ComponentId
  }

data MemState =
  MemState { addressLookup :: [MemorySource]
           }

data MemCommand = Register MemorySource
                | Read     Int
                | forall a . Typeable a => Write Int a
  deriving Typeable

data MemoryManager = MemoryManager
