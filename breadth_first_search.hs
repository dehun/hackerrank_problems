import Data.Functor
import Control.Applicative
import Data.Maybe
import Data.List
import Control.Monad    
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Sequence as Seq    



expand_node :: Seq.Seq (Int, Int) -> M.Map Int (S.Set Int) -> S.Set Int -> M.Map Int Int -> M.Map Int Int
expand_node queue joints expanded distances
    | Seq.length queue == 0 = distances
    | otherwise =
    let
        (ex, dst) = Seq.index queue 0
        ex_joints = maybe S.empty id (M.lookup ex joints)
        next_expandables = S.difference ex_joints expanded
        next_expanded = S.unions [(S.fromList [ex]), ex_joints,  expanded]
        next_queue_suffix = Seq.fromList $ S.toList $ S.map (\n -> (n, dst + 6)) next_expandables
        next_queue = (Seq.><) (Seq.drop 1 queue) next_queue_suffix
        best_distance = maybe dst id (M.lookup ex distances)
        next_distances = M.insert ex best_distance distances
    in expand_node next_queue joints next_expanded next_distances


distances_as_list :: [Int] -> M.Map Int Int ->  [Int]
distances_as_list nodes distances =
    map (\n -> maybe (-1) id (M.lookup n distances)) nodes
    

solve :: Int -> Int -> [(Int, Int)] -> [Int]
solve s n edges =
    let joints = foldl (\acc (ll, rr) ->
                            let ins a l r =
                                    case M.lookup l a of
                                      Nothing -> M.insert l (S.fromList [r]) a
                                      Just rs -> M.insert l (S.insert r rs) a
                            in ins (ins acc ll rr) rr ll
                       ) M.empty edges
        distances = expand_node (Seq.fromList [(s, 0)]) joints S.empty M.empty
        nodes = [1..n]
        nodes_except_start = filter (/=s) nodes
    in distances_as_list nodes_except_start distances


main = do
  q <- read <$> getLine
  mapM_ (\_ -> do
             [n, m] <- map read <$> words <$> getLine
             edges <- mapM (\_ -> (\[l, r] -> (l, r)) <$> map read <$> words <$> getLine) [1..m]
             s <- read <$> getLine
             putStrLn $ join $ intersperse " " $ map show ( solve s n edges) -- TODO: implement proper show
        ) [1..q]
