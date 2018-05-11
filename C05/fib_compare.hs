fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibStep (a,b) = (b,a+b)

step 0 _ a = a
step n f a = step (n-1) f (f a)

fib' n = fst $ step n fibStep (0,1)

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([]:xss) = transpose xss
transpose xs = [b|(b:_) <- xs] : transpose [bs|(_:bs) <- xs]

infixr 5 |*|
a |*| b = [[sum $ zipWith (*) ar br | br <- (transpose b)] | ar <- a]

unit = [[1,1],[1,0]]

fib'' 1 = unit
fib'' n | odd n = unit |*| fib'' (n-1)
        | otherwise = let x = fib'' (div n 2)
                      in x |*| x
