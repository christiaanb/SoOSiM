module SoOSiM.Components.Utils
  ( module Export
  , maybe'
  )
where

import Data.Maybe as Export
import Control.Monad as Export

maybe' :: Maybe a -> b -> (a -> b) -> b
maybe' m d f = maybe d f m
