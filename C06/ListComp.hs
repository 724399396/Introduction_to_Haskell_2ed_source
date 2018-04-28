filter' f xs = [x| x<-xs, f x]

series :: Int -> [Double]
series n = [1 / (2 * (fromIntegral k) + 1) * (-1)^k| k <- [0..n]]

series' :: Double -> Int -> [Double]
series' d n = [1 / (2 * (fromIntegral k) + 1) * (-1)^k * (d ^^ (2*(fromIntegral k) + 1)) | k <- [0..n]]

pi' n = let percise = n
      in 4 * sum (series' 0.5 percise) + 4 * sum (series' (1/3) percise)
