import           Data.Function
import           Data.Maybe
import           Data.Tree
import           Data.Tree.Zipper

directory :: Tree String
directory = Node "Home"
                 [Node "Picture" [Node "travel" [] , Node "family" []],
                  Node "Video" [Node "Fast and Furious" [], Node "True Lies" []],
                  Node "Music" [Node "My love" [], Node "Destiny" []]]

