import Data.Functor
import Data.Either    
import Debug.Trace    
import Control.Applicative
import Data.List
import Control.Monad
import qualified Data.Vector as V    

sss :: V.Vector Int -> Int -> Int -> Int -> Int
sss xs idx right_sum best_so_far 
    | idx == V.length xs = best_so_far
    | otherwise = 
    let current = (right_sum) * (idx + 1 )
        el = (V.!) xs idx 
    in if current <= best_so_far
       then best_so_far
       else sss xs (idx + 1) (right_sum - el) current
    

best_journey :: V.Vector Int -> Int
best_journey ms =
    sss ms 0 (V.sum ms) 0   
    
    
main = do
  t <- read <$> getLine 
  mapM_ (\_ -> do
           n <- read <$> getLine
           ms <-  map read <$> take n <$> words <$> getLine
           putStrLn $ show $ best_journey $ V.fromList $ sort ms
        ) [1..t]
