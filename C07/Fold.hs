-- Fold.hso
(+++) :: [a] -> [a] -> [a]
(+++) = foldr (:)

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x < y = x : y: ys
                | otherwise = y:insert x ys

isort :: Ord a => [a] -> [a]
isort xs = foldr insert [] xs

skip :: Eq a => a -> [a] -> [a]
skip x [] = [x]
skip x (y:ys) | x == y = (y:ys)
              | otherwise = x:y:ys

compress :: Eq a => [a] -> [a]
compress = foldr skip []

snoc :: a -> [a] -> [a]
snoc x = foldr (:) [x]

concat :: [[a]] -> [a]
concat = foldr (++) []

map' :: (a -> b) -> [a] -> [b]
map'  f = foldr (\l ls -> f l : ls) []

reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []

unwords' :: [String] -> String
unwords' [] =  ""
unwords' ws =  foldr1 (\w s -> w ++ ' ':s) ws

maximum', minimum' :: Ord a => [a] -> a
maximum'  = foldl1 max
minimum'  = foldl1 min

interLeave :: [a] -> [a] -> [a]
interLeave xs ys = help 0 xs ys
  where help 0 (x:xs) ys = x : (help 1 xs ys)
        help 1 xs (y:ys) = y : (help 0 xs ys)

interLeaveLists :: [[a]] -> [a]
interLeaveLists xs = foldr interLeave [] xs

removeOne :: Eq a => a -> [a] -> [a]
removeOne _ [] = []
removeOne x' (x:xs) | x' == x = xs
                    | otherwise = x:(removeOne x' xs)

removeDup :: Eq a => [a] -> [a]
removeDup xs = foldr removeOne xs xs
