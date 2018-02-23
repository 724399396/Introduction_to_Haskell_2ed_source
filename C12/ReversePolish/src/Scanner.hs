module Scanner where
import           Calculator
import           Data.Char

parse :: String -> LitOp
parse "+" = Right Plus
parse "-" = Right Minu
parse "*" = Right Mult
parse "/" = Right Divi
parse "pi" = Left (Const "pi")
parse "e" = Left (Const "e")
parse xs | null num = error "Excepted a number or constant"
         | otherwise = case rest of
                     ('.': r) -> let (float,r') = span isDigit r
                                    in Left (Val (read (num ++ "." ++ float) :: Float))
                     r        -> Left (Val (read num :: Float))
    where (num, rest) = span isDigit xs

scanExp :: String -> [LitOp]
scanExp = map parse . words
