module SoOSiM
  ( Sim
  , ComponentId
  , NodeId
  , ComponentInterface (..)
  , Input (..)
  , module SoOSiM.SimMonad
  , unmarshall
  , ignore
  )
where

import SoOSiM.SimMonad
import SoOSiM.Types    (Sim,ComponentId,NodeId,ComponentInterface(..)
                       ,Input(..))
import SoOSiM.Util     (unmarshall)

ignore ::
  ComponentInterface s
  => (Send s)
  -> Sim ()
ignore = const (return ())
