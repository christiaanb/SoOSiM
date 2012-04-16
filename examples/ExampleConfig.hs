module ExampleConfig where

import Control.Concurrent.STM
import Data.Maybe
import qualified Data.IntMap as IM
import qualified Data.Map    as Map
import SoOSiM
import SoOSiM.Simulator
import SoOSiM.Types
import UniqSupply
import Unique
import Text.PrettyPrint.HughesPJ

import HeatMap.Application
import HeatMap.Types
--import Fibbo

import MemoryManager
import MemoryManager.Types

import Scheduler
import Scheduler.Types

main :: IO ()
main = do
    supply <- mkSplitUniqSupply 'z'
    let (supply',supply'')       = splitUniqSupply supply
    let (node0id:component0id:_) = uniqsFromSupply supply'
    statusTV <- newTVarIO Running
    stateTV  <- newTVarIO Initializer
    bufferTV <- newTVarIO [Initialize]
    let emptyMeta = SimMetaData 0 0 0 Map.empty Map.empty
    emptyMetaTV   <- newTVarIO emptyMeta
    let component0CC             = CC component0id statusTV stateTV component0id bufferTV [] emptyMetaTV
    let node0                    = Node node0id NodeInfo Map.empty (IM.fromList [(getKey component0id,component0CC)]) IM.empty [component0id]
    let simState                 = SimState node0id component0id (IM.fromList [(getKey node0id,node0)]) supply'' Map.empty
    loop 0 simState
    return ()
  where
    loop ::
      Int
      -> SimState
      -> IO ()
    loop n simState = do
      putStrLn $ "Cycle: " ++ show n
      (fmap render $ showIO simState) >>= putStrLn
      simState' <- execStep simState
      c <- getChar
      case c of
        'n' -> loop (n+1) simState'
        _   -> return ()

data Initializer = Initializer

initializer ::
  Initializer
  -> ComponentInput
  -> SimM Initializer
initializer s Initialize = do
  nId <- getNodeId
  registerComponent (initState :: MemState)
  registerComponent (initState :: HMState)
  registerComponent (initState :: SchedulerState)
  _ <- createComponent (Just nId) Nothing "MemoryManager"
  _ <- createComponent (Just nId) Nothing "Scheduler"
  hmId <- createComponent (Just nId) Nothing "HeatMap"
  invokeNoWait Nothing hmId (toDyn Compute)
  yield s

initializer s _ = yield s

instance ComponentIface Initializer where
  initState          = Initializer
  componentName _    = "Initializer"
  componentBehaviour = initializer

class ShowIO a where
  showIO :: a -> IO Doc

instance ShowIO SimState where
  showIO (SimState _ _ nodes _ _) = do
    let ns = IM.elems nodes
    showIO ns

instance ShowIO a => ShowIO [a] where
  showIO [] = return empty
  showIO xs = fmap (foldl1 ($$)) $ mapM showIO xs


instance ShowIO Node where
  showIO (Node nId _ _ components mem order) = do
    componentsDoc <- showIO (IM.elems components)
    let retval = text "Node" <+> text (show nId) $+$ (nest 2 (text "components" <> colon <+> componentsDoc)) $+$ (nest 2 (text "valid mem addrs" <> colon <+> text (show $ IM.keys mem))) $+$ (nest 2 (text "order: " <> colon <+> text (show order)))
    return retval

instance ShowIO ComponentContext where
  showIO (CC cId statusTV stateTV _ bufferTV traceMsgs mdataTV) = do
    status  <- (readTVarIO statusTV) >>= showIO
    state   <- readTVarIO stateTV
    buffer  <- (readTVarIO bufferTV) >>= showIO
    mdata   <- (readTVarIO mdataTV) >>= showIO
    let traceMsgsDoc = foldl ($$) empty $ map text traceMsgs
    let retval = text (componentName state) <+> parens (text "id" <> colon <+> text (show cId)) <> colon <+> status $+$ (nest 2 (text "Pending events" <> colon <+> brackets (buffer))) $+$ (nest 2 (text "traceMsgs" <> colon <+> traceMsgsDoc)) $+$ (nest 2 mdata)
    return retval

instance ShowIO SimMetaData where
  showIO (SimMetaData running waiting idling received send) = do
    let receivedDoc = foldl ($+$) empty $ map (\(k,e) -> text (show k) <> colon <+> text (show e))  (Map.toList received)
    let sendDoc = foldl ($+$) empty $ map (\(k,e) -> text (show k) <> colon <+> text (show e))  (Map.toList send)

    let retval = text "cycles running" <> colon <+> text (show running) $+$
                 text "cycles waiting" <> colon <+> text (show waiting) $+$
                 text "cycles idling"  <> colon <+> text (show idling)  $+$
                 text "total cycles"  <> colon <+> text (show $ running + waiting + idling) $+$
                 text "Received Msgs" <> colon <+> receivedDoc $+$
                 text "Send Msgs" <> colon <+> sendDoc
    return retval

instance ShowIO (ComponentStatus s) where
  showIO Idle                  = return $ text "Idle"
  showIO Running               = return $ text "Running"
  showIO (WaitingForMsg cId _) = return $ text "Waiting for" <> colon <+> text (show cId)

instance ShowIO ComponentInput where
  showIO = return . text . show
