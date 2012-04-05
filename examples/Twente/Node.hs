{-# LANGUAGE DeriveDataTypeable #-}
module Node where

import SoOSiM

data NodeDef = NodeDef
  { nodeId   :: NodeId
  , nodeArch :: Architecture
  }
 deriving (Typeable)

type Architecture = String
