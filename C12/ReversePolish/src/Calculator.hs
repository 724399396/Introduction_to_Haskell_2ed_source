module Calculator where
import           Control.Monad.State

data Lit = Val Float | Const String deriving (Show, Eq)

data Op = Plus | Minu| Mult | Divi deriving (Eq, Show)

type LitOp = Either Lit Op
type Stack = [Lit]

evaluate :: Op -> Lit -> Lit -> State Stack ()
evaluate op (Val f1) (Val f2) = case op of
                                Plus  -> push $ lv (f1+f2)
                                Minu  -> push $ lv (f1-f2)
                                Mult  -> push $ lv (f1*f2)
                                Divi  -> push $ lv (f1/f2)


lv :: Float -> Lit
lv x = Val x

pop :: State Stack Lit
pop = state $ \xs -> case xs of
                              []     -> error "Number stack underflow"
                              (h:hs) -> (h,hs)

push :: Lit -> State Stack ()
push (Const "pi") = push $ lv 3.1415926
push  (Const "e" ) = push $ lv 2.7182812
push  (Const c )   = error $ "Unkown Constant"++c
push   a         = state $ \xs -> ((), a:xs)

pushIn  :: LitOp -> State Stack ()
pushIn (Left    num) = push num
pushIn (Right op) = do
  n1 <- pop
  n2 <- pop
  evaluate op n2 n1

calc :: [LitOp] -> State Stack Lit
calc [] = pop
calc (t:ts) = do
        pushIn t
        calc ts

inits :: [Lit]
inits = []
