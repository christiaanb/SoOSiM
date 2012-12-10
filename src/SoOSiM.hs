module SoOSiM
  ( module SoOSiM.SimMonad
  -- * SoOSiM API Types
  , ComponentInterface (..)
  , Input (..)
  , Sim
  , ComponentId
  , ComponentName
  , NodeId
  -- * Imported Types
  , Typeable
  , Dynamic
  -- * Progress The Simulator
  , initSim
  , tick
  -- * Utility Functions
  , ignore
  , unmarshall
  , returnAddress

  )
where

import Data.Dynamic          (Dynamic)
import Data.Typeable         (Typeable)
import SoOSiM.SimMonad
import SoOSiM.Simulator      (initSim,tick)
import SoOSiM.Simulator.Util (returnAddress)
import SoOSiM.Types          (ComponentId,ComponentInterface(..),Input(..)
                             ,NodeId,Sim,ComponentName)
import SoOSiM.Util           (unmarshall)

ignore ::
  a
  -> Sim ()
ignore = const (return ())
