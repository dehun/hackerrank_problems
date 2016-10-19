import Data.Functor
import qualified Data.Vector as V
import Control.Monad
import Data.Maybe    
import qualified Data.List as L
import qualified Data.Set as S
import Debug.Trace    
    

data Cell = Tree
          | Free
          | Key
          | Start
            deriving (Show, Eq)

char_to_cell :: Char -> Cell
char_to_cell 'X' = Tree
char_to_cell '.' = Free
char_to_cell '*' = Key
char_to_cell 'M' = Start


solve :: Int -> Int -> V.Vector (V.Vector Cell) -> Int
solve n m grid =
    let at_pos (y,x) = (V.!) ((V.!) grid y) x
        start_pos = fromJust $ L.find (\p -> Start == at_pos p) [(y, x) | y <- [0..n-1], x <- [0..m-1]]
        validate_pos (y, x) = if (y >= 0) && (y < n) && (x >= 0)
                              && (x < m) && (at_pos (y, x) /= Tree)
                              then Just (y, x)
                              else Nothing
        neighbours (y, x) = catMaybes $ L.filter isJust
                            (L.map (\p -> validate_pos p)
                                     [ (y + 1, x)
                                     , (y - 1, x)
                                     , (y, x + 1)
                                     , (y, x - 1)])
        bfs [] visited = Nothing
        bfs ((p, lucky_count):queue) visited =
            let
                filtered_neighbours = filter (\np -> not $ S.member np visited) (neighbours p)
                next_lucky_count = lucky_count + (if (L.length filtered_neighbours) >= 2
                                                  then 1
                                                  else 0) :: Int                                      
                neighbours_with_luck = [(np, next_lucky_count) | np <- filtered_neighbours]
                next_visited = S.insert p visited
            in if S.member p visited
               then bfs queue next_visited 
               else if at_pos p == Key
                    then Just lucky_count
                    else --trace ("next" ++ show p ++ " " ++ show lucky_count) $
                              bfs (queue ++ neighbours_with_luck) next_visited
    in fromJust $ bfs [(start_pos, 0)] S.empty


main = do
  t <- read <$> getLine
  mapM_ (\_ -> do
           [n, m] <- map read <$> take 2 <$> words <$> getLine
           rows <- mapM (\_ ->  getLine) [1..n]
           k <- read <$> getLine
           let grid = V.map (\r -> V.map char_to_cell (V.fromList r)) (V.fromList rows)
           putStrLn $ 
                    if k == solve n m grid
                    then "Impressed"
                    else "Oops!"
           ) [1..t]
  
