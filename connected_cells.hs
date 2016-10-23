import Data.Functor
import Data.Monoid
import Debug.Trace
import Control.Monad.Writer    
import qualified Data.Vector as V
import qualified Data.Set as S


solve :: Int -> Int -> V.Vector (V.Vector Int) -> Int
solve n m rows =
    let at_pos (y, x) = (V.!) ((V.!) rows  y) x
        all_poses = [(y, x) | y <- [0..n-1], x <- [0..m-1]]
        all_ones = S.fromList (filter (\p -> at_pos p == 1) all_poses)
        neighbours (py, px) = [ (y, x) |
                                y <- [py, py + 1, py - 1]
                              , x <- [px, px + 1, px - 1]
                              , (y, x) /= (py, px)]
        validate_pos (py, px) = (px >= 0) && (py >= 0) && (py < n) && (px < m)
        dfs :: (Int, Int) -> S.Set (Int, Int) -> Writer (Sum Int) (S.Set (Int, Int))
        dfs (py, px) visited =
            let
                valid_next_pos pos = validate_pos pos
                                     && not (S.member pos visited)
                                     && at_pos pos == 1
                next_poses = filter valid_next_pos $ neighbours (py, px)
                next_visited = S.union visited (S.fromList next_poses) 
            in foldM (\acc np -> do
                        tell 1
                        dfs np acc
                     ) next_visited next_poses
    in fst (S.foldl (\(max_group, visited) p ->
                          if S.member p visited
                          then (max_group, visited)
                          else
                              let (new_visited, group) = runWriter (dfs p (S.insert p visited))
                              in (max (1 + getSum group) max_group, new_visited)
                     ) (0, S.empty) all_ones)

    
    

main = do
  n <- read <$> getLine
  m <- read <$> getLine
  rows <- V.mapM (\_ -> V.fromList <$> map read <$> words <$> getLine) (V.fromList [1..n])
  putStrLn $ show $ solve n m rows
       
