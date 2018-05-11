import           Data.List (nub, permutations, tails)

data Exp = Val Double
         | Plus Exp Exp
         | Sub  Exp Exp
         | Mult Exp Exp
         | Div  Exp Exp deriving (Show,Eq)

eval :: Exp -> Double
eval (Val  a )   = a
eval (Plus a b ) = eval a + eval b
eval (Sub  a b ) = eval a - eval b
eval (Mult a b ) = eval a * eval b
eval (Div  a b ) = eval a / eval b

showExp :: Exp -> String
showExp (Val a)    = show a
showExp (Plus a b) = "("++showExp a ++ "+" ++ showExp b++")"
showExp (Sub  a b) = "("++showExp a ++ "-" ++ showExp b++")"
showExp (Mult a b) = "("++showExp a ++ "*" ++ showExp b++")"
showExp (Div  a b) = "("++showExp a ++ "/" ++ showExp b++")"

divide :: [a] -> [([a],[a])]
divide xs = [(take n xs ,drop n xs)| n <- [1..(length xs -1)]]

buildExpressions :: ([Exp],[Exp]) -> [Exp]
buildExpressions (es1,es2) = [op e1 e2 |e1<-es1, e2<- es2,
                                        op <- [Plus, Sub, Mult, Div]]

toExpressions :: [Double] -> [Exp]
toExpressions [] = []
toExpressions [x] = [Val x]
toExpressions xs = concat [buildExpressions (toExpressions l,
                     toExpressions r)| (l,r) <- divide xs ]

generate :: [Double] -> [Exp]
generate ns = concatMap toExpressions (permutations ns)

twentyfour :: [Double] -> [String]
twentyfour ns = [showExp x | x <- generate ns, eval x == 24.0 ]

twentyfour' :: [Double] -> [String]
twentyfour' ns = [showExp' x | x <- generate ns, eval x == 24.0 ]

data Priority = Low | High deriving (Eq)

showExp' :: Exp -> String
showExp' e = help e Low
  where
    help (Val a) _    = show a
    help (Plus a b) p | p == High = "("++help a Low ++ "+" ++ help b Low++")"
                       | otherwise = help a Low ++ "+" ++ help b Low
    help (Sub  a b) p | p == High = "("++help a Low ++ "-" ++ help b Low++")"
                       | otherwise = help a Low ++ "-" ++ help b Low
    help (Mult a b) _ = help a High ++ "*" ++ help b High
    help (Div  a b) _ = help a High ++ "/" ++ help b High

singleSolution = [x | x <- tails [1..13], length (nub (twentyfour x)) == 1]
