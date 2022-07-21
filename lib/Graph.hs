{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE FlexibleInstances #-}
module Graph (Graph(..)) where
import qualified Data.Aeson as A
    (FromJSON, ToJSON, Value (Null, Object, String), fromJSON, parseJSON,
    toJSON)
import           Data.Map
    (Map, fromList)
import qualified Data.Text  as T

data Graph a
  = Object (Map String (Graph a))
  | Leaf a
  deriving (Foldable)

instance Functor Graph where
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Object m) = Object $ f' <$> m
    where
      f' (Leaf a)   = Leaf $ f a
      f' (Object m) = Object $ fmap f' m

instance Traversable Graph where
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Object m) = Object <$> traverse f' m
    where
      f' (Leaf a)    = traverse f (Leaf a)
      f' (Object m') = traverse f (Object m')

instance A.ToJSON a => A.ToJSON (Graph a) where
  toJSON (Leaf l)   = A.toJSON l
  toJSON (Object m) = A.toJSON m

instance A.FromJSON (Graph String) where
  parseJSON (A.String text) = Leaf <$> A.parseJSON (A.String text)
  parseJSON (A.Object obj)  = Object <$> A.parseJSON (A.Object obj)
  parseJSON _               = (pure . Leaf) ""
