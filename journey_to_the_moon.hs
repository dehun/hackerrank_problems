import Data.List
import Debug.Trace    
import Data.Functor
import Data.Maybe    
import qualified Data.Map as M
import qualified Data.Set as S    


findClusters :: [(Int, Int)] -> S.Set Int -> [S.Set Int]
findClusters sedges start_nodes = {-# SCC "mean" #-} 
    let
        nodes = start_nodes
        jedges = M.fromSet  (\n ->
                                       S.fromList $ foldl (\acc e ->
                                                  if fst e == n
                                                  then snd e : acc
                                                  else if snd e == n
                                                       then fst e : acc
                                                       else acc
                                             ) [n] sedges
                                  ) start_nodes
        exploreNode n old_explored_nodes =
            let
                all_joints = (fromJust $ M.lookup n jedges) :: S.Set Int 
                unexplored_joints = S.difference all_joints explored_nodes
                explored_nodes = S.insert n old_explored_nodes
            in  S.foldl (\ex n -> exploreNode n ex)
                   (S.insert n explored_nodes)
                   unexplored_joints
            
    in snd (S.foldl (\(explored_nodes, clusters) n ->
                  if S.member n explored_nodes
                  then (explored_nodes, clusters)
                  else (S.union explored_nodes (exploreNode n S.empty)
                       , clusters ++ [exploreNode n S.empty])
             ) (S.empty, []) nodes)

combinations :: [Int] -> Int
combinations (x:xs) = (sum $ map (\y -> x*y) xs) + combinations xs
combinations [] = 0

    
solve :: [(Int, Int)] -> [Int] -> Int
solve start_edges start_nodes =
    let
        connected_nodes = S.union
                  (S.fromList (map fst start_edges))
                  (S.fromList (map snd start_edges))
        singles =  S.difference (S.fromList start_nodes) connected_nodes
        nsingles = S.size singles
        nsingles_leftover = sum [0..nsingles - 1]
        clusters = findClusters start_edges connected_nodes
        cluster_lenghts = map S.size clusters
    in
      combinations cluster_lenghts + nsingles * (sum cluster_lenghts) + nsingles_leftover 
    
    
tuplify (x:y:[]) = (x, y)


main = do
  [n, k] <- map read <$> words <$> getLine :: IO [Int]
  edges <- mapM (\_ -> tuplify <$> map read <$> take 2 <$> words <$> getLine ::IO (Int, Int)) [1..k]
  putStrLn $ show $ solve edges [0..n-1]
