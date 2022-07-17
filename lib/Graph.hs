{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE DeriveFoldable #-}
module Graph where
import Data.Map (Map, fromList)


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

-- traverse f Empty = pure Empty
-- traverse f (Leaf x) = Leaf <$> f x
-- traverse f (Node l k r) = Node <$> traverse f l <*> f k <*> traverse f r
instance Traversable Graph where
  traverse _f Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Object m) = Object <$> traverse f' m
    where
      f' Empty = pure Empty
      f' (Leaf a) = traverse f (Leaf a)
      f' (Object m') = traverse f (Object m')
