{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}
class (Num a, Num b) => GPlus a b where
  type SumType a b :: *
  plus :: a -> b -> SumType a b

instance GPlus Int Float where
  type SumType Int Float = Float
