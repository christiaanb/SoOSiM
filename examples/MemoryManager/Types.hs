{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
module MemoryManager.Types where

import SoOSiM

data MemoryManager = MemoryManager

data MemorySource
  = MemorySource
  { baseAddress :: Int
  , scope       :: Int
  , sourceId    :: Maybe ComponentId
  }
  deriving Show


data MemState =
  MemState { addressLookup :: [MemorySource]
           }
  deriving Show

data MemCommand = Register Int Int (Maybe ComponentId)
                | Read     Int
                | forall a . Typeable a => Write Int a
  deriving Typeable
