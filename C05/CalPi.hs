series c = [(1/((fromIntegral x)*2-1))*(c ^^ (fromIntegral x))*((-1) ^ (x-1)) | x <- [1..]]

myPi accur = 4 * (sum $ take accur $ series 0.5) + 4 * (sum $ take accur $ series (1/3))
