module MemoryManager.Util where

import Data.Maybe
import SoOSiM

import MemoryManager.Types

checkAddress ::
  [MemorySource]
  -> Int
  -> MemorySource
checkAddress sources addr = case (filter containsAddr sources) of
    []    -> error "address unknown"
    (x:_) -> x
  where
    containsAddr (MemorySource base sc _) = base <= addr && addr < sc
