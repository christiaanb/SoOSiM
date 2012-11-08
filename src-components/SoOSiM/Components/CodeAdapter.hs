{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
module SoOSiM.Components.CodeAdapter where

import SoOSiM
import SoOSiM.Components.Types

data CodeAdapter = CodeAdapter

data Data = Data deriving Typeable

data CA_Cmd = AdaptCode Code DestinationArchitecture
            | AdaptContext ThreadContext DestinationArchitecture
            | DataStandardisation Data
            | DataNativisation Data
            deriving Typeable

data CA_Msg = CA_AC (Maybe AdaptedCode)
            | CA_SD StandarData
            | CA_ND NativeData
            deriving Typeable

data AdaptedCode = AdaptedCode deriving Typeable
data StandarData = StandarData deriving Typeable
data NativeData  = NativeData deriving Typeable

data CA_State = CA_State

instance ComponentInterface CodeAdapter where
  type State CodeAdapter   = CA_State
  type Receive CodeAdapter = CA_Cmd
  type Send CodeAdapter    = CA_Msg
  initState              = const (CA_State)
  componentName          = const ("Code Adapter")
  componentBehaviour     = const codeAdapter

codeAdapter ::
  CA_State
  -> Input CA_Cmd
  -> Sim CA_State
codeAdapter caState (Message (AdaptCode c d) retAddr) = do
  ac <- case codeType c of
    SourceCode -> do
      case destinationType d of
        RealArchitecture -> do
          ac <- adaptCodeFromCodeToPlatform c d
          return (Just ac)
        IRArchitecture   -> do
          ac <- adaptCodeFromeCodeToIR c
          return (Just ac)
    IR -> do
      case destinationType d of
        RealArchitecture -> do
          ac <- adaptCodeFromIRToPlatform c d
          return (Just ac)
        _ -> return Nothing
    Binary -> do
      case destinationType d of
        RealArchitecture -> do
          ac <- adaptCodeFromBinaryToPlatfrom c d
          return (Just ac)
        IRArchitecture   -> do
          ac <- adaptCodeFromBinaryToIR c
          return (Just ac)

  respond CodeAdapter retAddr (CA_AC ac)
  yield caState

codeAdapter caState (Message (DataStandardisation d) retAddr) = do
  -- doAllTheWorkToStandardisiteInputData
  sd <- compute 4 StandarData
  respond CodeAdapter retAddr (CA_SD sd)
  yield caState

codeAdapter caState (Message (DataNativisation d) retAddr) = do
  -- doAllTheWorkToTransformStandardisiteInputDataToNativeRepresentation
  nd <- compute 4 NativeData
  respond CodeAdapter retAddr (CA_ND nd)
  yield caState

codeAdapter caState _ = yield caState

adaptCodeFromCodeToPlatform c d = do
  -- doNecessaryTransformations(...);
  ac <- compute 4 AdaptedCode
  return ac

adaptCodeFromeCodeToIR c = do
  -- doNecessaryTransformations(...);
  ac <- compute 4 AdaptedCode
  return ac

adaptCodeFromIRToPlatform c d = do
  -- doNecessaryTransformations(...);
  ac <- compute 4 AdaptedCode
  return ac

adaptCodeFromBinaryToPlatfrom c d = do
  -- doNecessaryTransformations(...);
  ac <- compute 4 AdaptedCode
  return ac

adaptCodeFromBinaryToIR c = do
  -- doNecessaryTransformations(...);
  ac <- compute 4 AdaptedCode
  return ac
