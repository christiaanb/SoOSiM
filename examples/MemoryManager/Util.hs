module MemoryManager.Util where

import Data.Maybe
import SoOSiM

import MemoryManager.Types

identifyAddress :: Dynamic -> Maybe Int
identifyAddress d = case (fromDynamic d) of
  Just (Write i _) -> Just i
  Just (Read i)    -> Just i
  Nothing          -> Nothing

memCommand :: Dynamic -> MemCommand
memCommand = fromJust . fromDynamic
