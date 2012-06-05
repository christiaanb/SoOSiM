module SoOSiM
  ( module SoOSiM.SimMonad
  , Sim
  , ComponentId
  , NodeId
  , ComponentInterface (..)
  , Input (..)
  , Typeable
  , ignore
  , tick
  , unmarshall
  , returnAddress
  )
where

import Data.Typeable    (Typeable)
import SoOSiM.SimMonad
import SoOSiM.Simulator (tick)
import SoOSiM.Simulator.Util (returnAddress)
import SoOSiM.Types     (ComponentId,ComponentInterface(..),Input(..),NodeId
                        ,Sim)
import SoOSiM.Util      (unmarshall)

ignore ::
  ComponentInterface s
  => s
  -> (Send s)
  -> Sim ()
ignore _ = const (return ())
