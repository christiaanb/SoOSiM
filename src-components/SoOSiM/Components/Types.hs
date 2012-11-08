module SoOSiM.Components.Types where

import SoOSiM

data Code = Code { codeType :: CodeType }
data CodeType = SourceCode
              | IR
              | Binary

data Destination = Destination
  { dLoc  :: NodeId
  , dArch :: DestinationArchitecture
  }

data DestinationArchitecture = DestinationArchitecture
  { destinationType :: DestinationType
  }

data DestinationType
  = RealArchitecture
  | IRArchitecture

data ThreadContext = ThreadContext

type Memory = Int

data ThreadID = ThreadID
  { threadMem :: Memory
  }

type ProcessId = ComponentId
data Process = Process


data AppData = AppData [(BlockName, BlockCode, ResourceDescription)]

type BlockName = String
type BlockId   = Int
type BlockCode = Code
type ResourceDescription = ()
