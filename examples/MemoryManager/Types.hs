{-# LANGUAGE DeriveDataTypeable #-}
module MemoryManager.Types where

import SoOSiM

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
                | Write    Int Dynamic
  deriving Typeable
