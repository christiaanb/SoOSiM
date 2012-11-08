{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies       #-}
module SoOSiM.Components.ApplicationHandler where

import SoOSiM
import SoOSiM.Components.Types

type AH_State = ()

data AH_Cmd
  = GetAppData String
  deriving Typeable

data AH_Msg
  = AH_AppData (Maybe AppData)
  deriving Typeable

data ApplicationHandler = ApplicationHandler

instance ComponentInterface ApplicationHandler where
  type State ApplicationHandler   = AH_State
  type Receive ApplicationHandler = AH_Cmd
  type Send ApplicationHandler    = AH_Msg
  initState                       = const ()
  componentName                   = const ("Application Handler")
  componentBehaviour              = const applicationHandler

applicationHandler ::
  AH_State
  -> Input AH_Cmd
  -> Sim AH_State
applicationHandler s _ = yield s
