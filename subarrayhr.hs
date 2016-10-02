import Data.List    
import Data.Maybe    
import Data.Functor    
import Control.Monad
import Control.Applicative


max_subarray :: [Integer] -> Integer
max_subarray (h:s) =
    snd (foldl (\(prev_max, prev_big) x ->
                    let max_ending_here = max (prev_max + x) x
                    in (max_ending_here, max prev_big max_ending_here)
          ) (h, h) s)

max_ra :: [Integer] -> Integer
max_ra s =
    let
        noneg_start = dropWhile (\x -> x < 0) s
        noneg_sum = if noneg_start /= []
                    then foldl (\acc x -> if x > 0 then acc + x else acc) 0 noneg_start
                    else maximum s
    in noneg_sum
          

    
solve :: [Integer] -> (Integer, Integer)
solve s = (max_subarray s,  max_ra s)

main = do
  ncases <- read <$> getLine :: IO Integer
  cases <- mapM (\_ -> do
                     n <- read <$> getLine :: IO Integer
                     (map read <$> words <$> getLine)
                ) [1..ncases]
  mapM_ (\s -> putStrLn $ show (fst s) ++ " " ++ show (snd s)) (map solve cases)
