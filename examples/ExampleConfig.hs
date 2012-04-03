module HeatMap where

import Data.Maybe
import qualified Data.IntMap as IM
import qualified Data.Map    as Map
import SoOSiM
import SoOSiM.Simulator
import SoOSiM.Types
import UniqSupply
import Unique

main :: IO ()
main = do
    supply <- mkSplitUniqSupply 'z'
    let (supply',supply'')       = splitUniqSupply supply
    let (node0id:component0id:_) = uniqsFromSupply supply'
    let component0CC             = CC Running Initializer (error "no parent") [Initialize]
    let node0                    = Node node0id NodeInfo (Map.fromList [("Initializer",component0id)]) (IM.fromList [(getKey component0id,component0CC)]) IM.empty
    let simState                 = SimState node0id component0id (IM.fromList [(getKey node0id,node0)]) supply'' (Map.fromList [("Initializer",component0CC)])
    loop 0 simState
    return ()
  where
    loop ::
      Int
      -> SimState
      -> IO ()
    loop n simState = do
      putStrLn $ "Cycle: " ++ show n
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
initializer s Initialize =
  error "make a configuration here"

initializer s _ = yield s

instance ComponentIface Initializer where
  initState          = Initializer
  componentName _    = "Initializer"
  componentBehaviour = initializer
