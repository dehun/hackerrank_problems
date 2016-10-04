import Data.Functor
import Data.Either    
import Debug.Trace    
import Control.Applicative
import Data.List
import Control.Monad    


best_journey :: [Int] -> Int
best_journey ms =
    let res = foldM (\(left_sum, right_sum, best_so_far) m ->
                             if left_sum * right_sum < best_so_far
                             then Left best_so_far
                             else traceShowId $ Right (left_sum + 1, right_sum - m, left_sum * right_sum)
          ) (1, sum ms, 0) ms
    in case res of
         Left r -> r
         Right (_, _, r) -> r


main = do
  t <- read <$> getLine 
  mapM_ (\_ -> do
           n <- read <$> getLine
           ms <-  map read <$> take n <$> words <$> getLine
           putStrLn $ show $ best_journey $ sort ms
        ) [1..t]
