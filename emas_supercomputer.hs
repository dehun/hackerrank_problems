import Control.Applicative
import qualified Data.Set
import Data.List
import Data.Maybe

expand_crosses :: [[Char]] -> (Integer, Integer) -> [[(Integer, Integer)]]
expand_crosses rows pos  =
   let rowsHeight = (fromIntegral $ length rows) :: Integer
       rowsWidth = (fromIntegral $ length $ head rows) :: Integer
       is_good_pos :: (Integer, Integer) -> Bool
       is_good_pos p = and [ fst p < rowsWidth
                           , fst p >= 0
                           , snd p < rowsHeight
                           , snd p >= 0
                           , (rows !! fromIntegral (snd p) !! fromIntegral (fst p)) == 'G'] -- TODO: what first?
       inner_expand_crosses :: Integer -> Maybe [(Integer, Integer)]
       inner_expand_crosses 0 = if is_good_pos pos
                                then Just [pos]
                                else Nothing
       inner_expand_crosses n =
           if all is_good_pos expansions
           then (++) <$> (Just expansions) <*> (concat <$> (mapM inner_expand_crosses [0..n - 1]))
           else Nothing
           where expansions = [ (fst pos + n, snd pos)
                              , (fst pos - n, snd pos)
                              , (fst pos, snd pos + n)
                              , (fst pos, snd pos - n) ] :: [(Integer, Integer)]
   in map (Data.Set.toList . Data.Set.fromList . fromJust)
          (filter (\l -> l /= Nothing) (map inner_expand_crosses [0..(min rowsWidth rowsHeight)]))
    
    
find_all_crosses :: [[Char]] -> [[(Integer, Integer)]]
find_all_crosses rows =
    let rowsHeight = (fromIntegral $ length rows) :: Integer
        rowsWidth = (fromIntegral $ length $ head rows) :: Integer
    in
    concatMap (expand_crosses rows) [(x, y) | x <- [0..rowsWidth], y <- [0..rowsHeight]]


solve :: [[Char]] -> Integer
solve rows =
    let all_crosses = find_all_crosses rows
        all_pairs = [(l, r) | l <- all_crosses, r <- all_crosses] 
        non_overlapping_pairs =
            filter (\(l, r) ->
                        (Data.Set.intersection (Data.Set.fromList l) (Data.Set.fromList r)) == Data.Set.empty)
            all_pairs
    in maximum (map (\(l, r) -> fromIntegral (length l) * fromIntegral (length r)) non_overlapping_pairs)

main = do
  [n, m] <- map read <$> (words <$> getLine)
  rows <- mapM (\x -> take m <$> getLine) [1..n]
  putStrLn $ show $ solve rows

