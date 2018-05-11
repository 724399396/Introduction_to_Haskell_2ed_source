interLeave xs ys = help 0 xs ys
  where help 0 (x:xs) ys = x : help 1 xs ys
        help 1 xs (y:ys) = y : help 0 xs ys

--foldr f acc [] = acc
--foldr f acc (x:xs) = f x (foldr f acc xs)

interLeaveLists :: [[a]] -> [a]
interLeaveLists = foldr interLeave []

removeDup :: Eq a => [a] -> [a]
removeDup = foldr skipDup []
  where skipDup x [] = [x]
        skipDup x (x':xs') = if x == x'
                             then x:xs'
                             else x:x':xs'
