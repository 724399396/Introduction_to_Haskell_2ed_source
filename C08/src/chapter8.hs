import Data.List (group)

brace :: Int -> [String]
brace 0 = [""]
brace n = let left = (brace (n-1))
  in left >>= \x -> map head $ group ['(':x++")", "()"++x, x++"()"]
