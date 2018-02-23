import Control.Monad

powerSet = choice (\x -> [True, False])

choice :: (a -> [Bool]) -> [a] -> [[a]]
choice _ [] = [[]]
choice f (x:xs) = [if choose then x:ys else ys|choose <- f x, ys <- choice f xs]

safeDiv :: Double -> Double -> Maybe Double
safeDiv a 0 = Nothing
safeDiv a b = Just (a / b)
