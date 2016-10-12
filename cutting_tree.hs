import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Vector as V    
import Debug.Trace
import Data.Functor
import Data.List
import Data.Maybe    


data TNode = TNode { tn_value :: Int
                   , tn_subnodes :: [TNode]
                   , tn_sum_value :: Int}
           deriving (Show, Eq)

build_node :: Int -> V.Vector Int -> M.Map Int (S.Set Int) -> S.Set Int -> TNode
build_node idx values joints built_nodes =
    let
        ix = idx
        connected_nodes = fromJust $ M.lookup idx joints :: S.Set Int
        nodes_to_built = S.difference connected_nodes built_nodes
        new_nodes_to_built = S.unions [S.fromList [idx], nodes_to_built, built_nodes]
        subnodes = map (\subidx -> build_node subidx values joints new_nodes_to_built) (S.toList nodes_to_built)
        node_value =  (V.!) values (idx - 1)
        sum_value = node_value + (sum $ map tn_sum_value subnodes)
    in TNode node_value subnodes sum_value

build_tree :: V.Vector Int -> M.Map Int (S.Set Int) -> TNode
build_tree values joints = build_node 1 values joints S.empty

tn_sum :: TNode -> Int
tn_sum tn =
    tn_value tn + sum (map tn_sum (tn_subnodes tn))

min_search :: TNode -> Int -> Int
min_search tn whole_sum =
    let min_node_cut stn = min
                           (abs (whole_sum - 2 * (tn_sum_value stn)))
                           (min_search stn whole_sum)
    in
      case tn_subnodes tn of
        [] -> whole_sum
        _ -> minimum [min_node_cut stn | stn <- tn_subnodes tn]
             

solve :: TNode -> Int                           
solve tree =
    let whole_tree_sum = tn_sum tree
    in min_search tree whole_tree_sum
    
    

main = do
  n <- read <$> getLine :: IO Int
  xs <- V.fromList <$> map read <$> words <$> getLine :: IO (V.Vector Int)
  joints <- S.fromList <$> (mapM (\_ -> (\[l, r] -> (l, r)) <$>map read <$> words <$> getLine )) [1..n - 1] 
  let joints_map = S.foldl (\acc (ll, rr) ->
                              let ins a l r =
                                      case M.lookup l a of
                                        Just ls -> M.insert l (S.insert r ls) a
                                        Nothing -> M.insert l (S.fromList [r]) a
                              in ins (ins acc ll rr) rr ll
                         )  M.empty joints
  putStrLn $ show $ solve $ build_tree xs joints_map
  
