import qualified Data.Map as M
import Data.Functor
import Data.Maybe

data TNode = TNode { tn_rng :: (Int, Int)
                   , tn_subrngs :: [TNode] }


build_tree :: M.Map Char [Int] -> TNode
build_tree occurances =
    let pairs = M.foldl (\acc occ ->
                             acc ++ [(l, r) | l <- occ, r <- occ, l < r]
                        ) [] occurances
    in undefined
    

solve :: String -> Integer
solve s =
    let occurances = foldl (\acc (idx, c) ->
                                case M.lookup c acc of
                                  Just occ -> M.insert c (idx:occ) acc
                                  Nothing -> M.insert c ([idx]) acc
                           ) M.empty (zip [1..length s] s)
        tnode = build_tree occurances
    in 0
    
    

main = do
  s <- getLine
  putStrLn $ show $ (solve s) `mod` (1000000000 + 7)
  
