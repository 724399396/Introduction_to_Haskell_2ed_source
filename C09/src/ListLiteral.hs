{-# LANGUAGE TypeFamilies, OverloadedLists #-}
import qualified Data.Foldable as DF (toList)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import GHC.Exts

instance (Ord a) => IsList (Set.Set a) where
  type Item (Set.Set a) = a
  fromList = Set.fromList
  toList = Set.toList

instance IsList (Seq.Seq a) where
  type Item (Seq.Seq a) = a
  fromList = Seq.fromList
  toList = DF.toList
