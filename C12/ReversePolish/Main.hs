module Main where
import Calculator (calc,inits,Lit (Val),LitOp)
import Scanner (scanExp)
import System.Environment (getArgs)
import Control.Monad.State (evalState)

calculate :: String -> Lit
calculate exp = (evalState.calc.scanExp) exp inits

main :: IO ()
main = do
        expr <- getLine
        print $ calculate expr
