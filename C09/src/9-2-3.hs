{-# LANGUAGE InstanceSigs #-}

data Tree a = Leaf a | Branch (Tree (a,a)) deriving (Show)

instance Functor Tree where
  fmap :: (a -> b) -> (Tree a) -> (Tree b)
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Branch x) = Branch (fmap (\(y,z) -> (f y, f z)) x)
