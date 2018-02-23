{-# LANGUAGE GADTs #-}
data Rect = Rect Double Double
data Circle = Circle Double

class HasArea t where
    area :: t -> Double

instance HasArea Rect where
         area (Rect a b) = a * b

instance HasArea Circle where
         area (Circle r) = pi * r * r

data Shape where
     Shape :: HasArea t => t -> Shape

instance HasArea Shape where
  area (Shape shape) = area shape

shapes = [Shape (Rect 2 3), Shape (Circle 1.5)]
