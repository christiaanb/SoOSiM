{-# LANGUAGE DeriveDataTypeable #-}
module CodeDeployer
   ( DeployerState
   , CodeDeployerMsg(..)
   )
  where

import Control.Monad.IfElse

import Code
import Node
import CodeAdapter
import SoOSiM

data DeployerState = DeployerState

type CodeDeployerDeps = ComponentId -- CodeAdapterId

data CodeDeployerMsg = DeployBlock String BlockCode NodeDef
 deriving (Typeable)

instance ComponentIface DeployerState where

 initState          = DeployerState

 componentName _    = "CodeDeployer"

 componentBehaviour state msg = do
   codeAdapterId <- componentLookup Nothing "CodeAdapter"
   maybe (return state) (componentBehaviourWithIds state msg) codeAdapterId

componentBehaviourWithIds :: DeployerState -> ComponentInput -> CodeDeployerDeps -> SimM DeployerState
componentBehaviourWithIds state (ComponentMsg caller contents) deps
  | Just (DeployBlock name code node) <- deployerAction
  = do rsp <- invoke Nothing deps (toDyn (Compile code (nodeArch node)))
       invoke Nothing caller (toDyn name)
       return state
  | otherwise
  = return state
 where deployerAction = fromDynamic contents
componentBehaviourWithIds state _ _ = return state
