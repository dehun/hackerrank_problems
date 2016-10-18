import Data.Functor
import Control.Monad
import Control.Applicative
import Data.Maybe    
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Debug.Trace    
    
import Data.Foldable    
import GHC.Enum    

data Edge = Edge {
      edge_sn :: Int
    , edge_en :: Int
    , edge_w :: Int
    } deriving (Show, Eq)    


build_edges :: [[Int]] -> M.Map Int [Edge]
build_edges joints = L.foldl (\acc j ->
                                let [jsn, jen, jw] = j :: [Int]
                                    ledge = Edge jsn jen jw
                                    redge = Edge jen jsn jw
                                    ins :: M.Map Int [Edge] -> Int -> Edge -> M.Map Int [Edge]
                                    ins a l e = case M.lookup l a of
                                                    Nothing -> M.insert l [e] a
                                                    Just edges -> M.insert l (e:edges) a
                                in ins (ins acc jsn ledge) jen redge
                           ) M.empty joints


dijkstra :: S.Set Int -> M.Map Int Int -> M.Map Int [Edge] -> M.Map Int Int
dijkstra to_visit distances edges
    | to_visit == S.empty = distances
    | otherwise = {-# SCC "djk" #-}
    let
        node_distance n = case M.lookup n distances of
                            Nothing -> maxBound :: Int
                            Just d -> d                                   
        next_node = minimumBy (\l r -> compare (node_distance l) (node_distance r)) to_visit
        next_traveled = M.lookup next_node distances
        next_edges = fromJust $ M.lookup next_node edges 
        next_distances = L.foldl
                         (\acc e ->
                              case next_traveled of
                                Nothing -> acc
                                Just next_traveled_val ->
                                    case M.lookup (edge_en e) acc of
                                      Nothing -> M.insert (edge_en e) (next_traveled_val + edge_w e) acc
                                      Just d -> if d < next_traveled_val + edge_w e
                                                then acc
                                                else M.insert (edge_en e) (next_traveled_val + edge_w e) acc)
                         distances next_edges
        next_to_visit = S.delete next_node to_visit
    in case next_traveled of
         Just _ -> dijkstra next_to_visit next_distances edges
         Nothing -> next_distances
       
              
build_distances :: [Int] -> Int -> M.Map Int [Edge] -> M.Map Int Int
build_distances nodes s edges =
    let to_visit = S.fromList nodes
        distances = M.fromList [(s, 0)]
    in dijkstra to_visit distances edges


show_distances :: Int -> Int -> M.Map Int Int -> String
show_distances s n distances =
    let arr = map (\nid -> case M.lookup nid distances of
                             Nothing -> show (-1)
                             Just d -> show d
                  ) [nid | nid <- [1..n], nid /= s]
    in join (L.intersperse " " arr)

          
solve :: Int -> Int -> [[Int]] -> String
solve s n joints =
    let edges = build_edges joints
        distances = build_distances [1..n] s edges
    in show_distances s n distances

main = do
  t <- read <$> getLine
  Control.Monad.mapM_ (\_ -> do
             [n, m] <- map read <$> take 2 <$> words <$> getLine
             joints <- mapM (\_ -> map read <$> take 3 <$> words <$> getLine ) [1..m]
             s <- read <$> getLine
             putStrLn $ solve s n joints
        ) [1..t]
