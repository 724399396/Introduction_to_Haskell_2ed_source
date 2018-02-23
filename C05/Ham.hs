merge :: (Num a, Ord a) => [a] -> [a] -> [a]
merge (x:xs) (y:ys) | x < y = x : (merge xs (y:ys))
                    | x > y = y : (merge (x:xs) ys)
                    | otherwise = x : (merge xs ys)

ham :: (Num a, Ord a) => [a]
ham = 1 : (merge (merge (map (*2) ham) (map (*3) ham)) (map (*5) ham))
