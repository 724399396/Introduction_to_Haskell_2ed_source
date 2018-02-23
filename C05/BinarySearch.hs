-- BinarySearch.hs
search :: (Ord a) => a -> [a] -> Bool
search a [] = False
search a xs | m < a = search a behind
            | m > a = search a front
            | otherwise = True
            where (front,m:behind) = splitAt (length xs `div` 2) xs

search' :: (Ord a) => a -> [a] -> [a]
search' _ [] = []
search' a xs | m < a = search' a behind
             | m > a = search' a front
             | otherwise = a : (search' a behind ++ search' a front)
             where (front, m:behind) = splitAt (length xs `div` 2) xs
