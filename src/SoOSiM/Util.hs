{-# LANGUAGE ScopedTypeVariables #-}
module SoOSiM.Util where

import Data.Dynamic (Typeable,Dynamic,fromDyn,toDyn)
import Data.IntMap  (IntMap,Key,member,adjust,insert)
import Data.Monoid  (Monoid (..))

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

unmarshall :: forall a . Typeable a => String -> Dynamic -> a
unmarshall msg d = fromDyn d
                 (error $  "unmarshal failed: expected value of type: "
                        ++ show resDyn
                        ++ ", received value of type: "
                        ++ show d
                        ++ "\nmessage:\n"
                        ++ msg
                 )
  where
    resDyn :: Dynamic
    resDyn = toDyn (undefined :: a)

maybe' :: Maybe a -> b -> (a -> b) -> b
maybe' Nothing b _  = b
maybe' (Just a) _ f = f a
