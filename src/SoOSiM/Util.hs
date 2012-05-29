module SoOSiM.Util
  ( module SoOSiM.Util
  , module Data.Dynamic
  )
where

import Data.Dynamic
import Data.IntMap
import Data.Monoid

class MonadUnique m where
  getUniqueM :: m Int

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
