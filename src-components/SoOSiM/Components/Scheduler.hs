{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies       #-}
module SoOSiM.Components.Scheduler where

import SoOSiM
import SoOSiM.Components.Types

type SD_State = ()

data SD_Cmd
  = Start String NodeId
  deriving Typeable

data SC_Msg
  = Started ProcessId
  deriving Typeable

data Scheduler = Scheduler

instance ComponentInterface Scheduler where
  type State Scheduler   = SD_State
  type Receive Scheduler = SD_Cmd
  type Send Scheduler    = SD_Msg
  initState              = const ()
  componentName          = const "Scheduler"
  componentBehaviour     = const scheduler

scheduler ::
  AH_State
  -> Input AH_Cmd
  -> Sim AH_State
scheduler s _ = yield s
