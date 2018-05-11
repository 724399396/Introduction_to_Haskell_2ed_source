import Data.List (minimumBy, transpose, isPrefixOf, intercalate, group)
import Data.Ord (comparing)

type Distance = Double
type Name = String
type Direction = String
type Weight = (Distance, Direction)

zipD :: [Name] -> [[Direction]]
zipD ns = [[start ++ "->" ++ end | end <- ns] | start <- ns]

zipW :: [[Distance]] -> [Name] -> [[Weight]]
zipW dss ns = zipWith zip dss $ zipD ns

splitBy spliter inp = help [] spliter inp
  where
    help acc _ [] = [acc]
    help acc pre l@(x:xs) = if isPrefixOf pre l
                     then acc : help [] pre (drop (length pre) l)
                     else help (acc ++ [x]) pre xs

tuplePlus :: Weight -> Weight -> Weight
tuplePlus (dis1, dir1) (dis2, dir2) = (dis1+dis2, intercalate "->" $ map head $ group $ splitBy "->" $ dir1 ++ "->" ++ dir2)

type RouteMap = [[Weight]]

step :: RouteMap -> RouteMap -> RouteMap
step a b =
  [[ minimumBy (comparing fst) $ zipWith tuplePlus ar br | br <- transpose b] |ar <- a]

i = 1 / 0

name = ["A", "B", "C", "D", "E"]

graph =
  [ [0, 6, 2, i, 7]
  , [6, 0, 3, i, i]
  , [2, 3, 0, 1, 5]
  , [i, i, 1, 0, 4]
  , [7, i, 5, 4, 0]
  ]

fix f x = let x' = f x
          in
          if map (map fst) x == map (map fst) x' then x else fix f x'

path :: [[Distance]] -> [Name] -> RouteMap
path dis ns = fix (step route) route
  where
    route = zipW dis ns

route = zipW graph name

iteration ::  Int -> (a -> a) -> a -> a
iteration 0 f x = x
iteration n f x = iteration (n-1) f (f x)

iteration' :: Int -> (a -> a -> a) -> a -> a
iteration' 0 f x = x
iteration' n f x | odd n = let half = iteration' ((n-1) `div` 2) f x
                           in f x (f half half)
                 | otherwise = let half = iteration' (n `div` 2) f x
                               in f half half
