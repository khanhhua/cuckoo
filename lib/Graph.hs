{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleInstances #-}
module Graph (Graph(..)) where
import Data.Map (Map, fromList)
import qualified Data.Aeson as A (ToJSON, toJSON, Value (Null, String, Object), FromJSON, parseJSON, fromJSON)
import qualified Data.Text as T

data Graph a
  = Empty
  | Object (Map String (Graph a))
  | Leaf a
  deriving (Foldable)

instance Functor Graph where
  fmap f Empty = Empty
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Object m) = Object $ f' <$> m
    where
      f' Empty = Empty
      f' (Leaf a) = Leaf $ f a
      f' (Object m) = Object $ fmap f' m

instance Traversable Graph where
  traverse _f Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Object m) = Object <$> traverse f' m
    where
      f' Empty = pure Empty
      f' (Leaf a) = traverse f (Leaf a)
      f' (Object m') = traverse f (Object m')

instance A.ToJSON a => A.ToJSON (Graph a) where 
  toJSON Empty      = A.Null
  toJSON (Leaf l)   = A.toJSON l
  toJSON (Object m) = A.toJSON m

instance A.FromJSON (Graph String) where
  parseJSON A.Null = pure Empty
  parseJSON (A.String text) = Leaf <$> A.parseJSON (A.String text)
  parseJSON (A.Object obj) = Object <$> A.parseJSON (A.Object obj)
  parseJSON _ = pure Empty
