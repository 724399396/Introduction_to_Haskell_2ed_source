import Data.List (insertBy, sortBy)
import Data.Ord (comparing)
import Data.Maybe (fromJust)

data HTree a = HLeaf a | HBranch (HTree a) (HTree a)

htree [(_, t)] = t
htree ((w1, t1):(w2, t2):left) = htree $ insertBy (comparing fst) (w1+w2, HBranch t1 t2) left

htree' [(_, t)] [] = t
htree' [] [(_, t)] = t
htree' [(w1,t1)] [(w1',t1')] = htree' [] [(w1+w1', HBranch t1 t1')]
htree' ((w1,t1):(w2,t2):left) [(w1',t1')] = if w2 > w1'
                                            then htree' ((w2,t2):left) [(w1+w1', HBranch t1 t1')]
                                            else htree' left [(w1',t1'), (w1+w2, HBranch t1 t2)]

serilize (HLeaf a) = [(a, "")]
serilize (HBranch l r) = [(x, '0':code) |(x, code) <- serilize l] ++
                         [(x, '1':code) |(x, code) <- serilize r]

huffman :: (Num w, Ord w) => [(a,w)] -> [(a,String)]
huffman freq = serilize $ htree $ sortBy (comparing fst) [(w, HLeaf a)|(a,w) <- freq]

input = (zip ['a'..] [0.4,0.3,0.1,0.1,0.06,0.04])

table = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

encrypto :: String -> String
encrypto str = concatMap encode str
  where encodeTable = huffman $ zip ['a'..] table
        encode x = maybe (x:[]) id $ lookup x encodeTable
