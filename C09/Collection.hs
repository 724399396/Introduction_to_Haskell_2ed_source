{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}
class Collection e ce | ce -> e where
  empty :: ce
  insert :: e -> ce -> ce
  member :: e -> ce -> Bool

instance Eq a => Collection a [a] where
  empty = []
  insert x xs = (x : xs)
  member = elem
