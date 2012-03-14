module SoOSiM.Util
  ( module SoOSiM.Util
  , module Data.IntMap
  , module Data.Dynamic
  )
where

import Data.Dynamic
import Data.IntMap
import Data.Maybe
import Data.Monoid

import SoOSiM.Types

identifyAddress :: Dynamic -> Maybe Int
identifyAddress = fmap addr . toMemCommand

memCommand :: Dynamic -> MemCommand
memCommand = fromJust . toMemCommand

toMemCommand :: Dynamic -> Maybe MemCommand
toMemCommand = fromDynamic

fromMemCommand :: MemCommand -> Dynamic
fromMemCommand = toDyn

addr :: MemCommand -> Int
addr (Read i)    = i
addr (Write i _) = i

adjustForce :: Monoid a => (a -> a) -> Key -> IntMap a -> IntMap a
adjustForce f k m = case (member k m) of
  True  -> adjust f k m
  False -> insert k (f mempty) m

mapAccumLM :: Monad m => (a -> b -> m (a,c)) -> a -> [b] -> m (a,[c])
mapAccumLM _ a []     = return (a,[])
mapAccumLM f a (x:xs) = do
  (a',y) <- f a x
  (a'',ys) <- mapAccumLM f a' xs
  return (a'',y:ys)
