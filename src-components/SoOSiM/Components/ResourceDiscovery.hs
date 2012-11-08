{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies       #-}
module SoOSiM.Components.ResourceDiscovery where

import SoOSiM
import SoOSiM.Components.Types

data ResourceDiscovery = ResourceDiscovery

type RD_State = ()

data RD_Cmd
  = FindResources Int ResourceDescription
  deriving Typeable

data RD_Msg
  = Suitable [NodeId]
  deriving Typeable

instance ComponentInterface ResourceDiscovery where
  type State ResourceDiscovery   = RD_State
  type Receive ResourceDiscovery = RD_Cmd
  type Send ResourceDiscovery    = RD_Msg
  initState                      = const ()
  componentName                  = const ("ResourceDiscovery Manager")
  componentBehaviour             = const resourceDiscovery

resourceDiscovery ::
  RD_State
  -> Input RD_Cmd
  -> Sim RD_State
resourceDiscovery s _ = yield s
