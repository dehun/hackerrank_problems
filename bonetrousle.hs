import Data.Functor
import Data.Maybe
import Data.List
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State    
import Debug.Trace
import qualified Data.Map as M    

    
solve :: [Int] -> Int -> Int -> Int -> State (M.Map (Int, Int, Int) (Maybe [Int])) (Maybe [Int])
solve xs n b k = do
      m <- get
      case M.lookup (n, b, k) m of
        Nothing ->
            do                   
              r <- --trace ("solving " ++ show xs ++ "; k=" ++ show k ++ "; n=" ++ show n ++ "; b=" ++ show b) $ --traceShowId $
                                solve_ xs n b k
--              put (M.insert (n, b, k) r m)
              return r
        Just r -> return $ r
       

solve_ :: [Int] -> Int -> Int -> Int -> State (M.Map (Int, Int, Int) (Maybe [Int])) (Maybe [Int])
solve_ [] n b k = if b == 0 && n == 0 then return $ Just [] else return Nothing
solve_ (x:xs) n b k
    | (n <= 0) = return Nothing
    | b * (x + x + b - 1) `div` 2 > n = return Nothing
    | k < b = return Nothing
    | (k) * (x + x + k) `div` 2 < n = return Nothing
    | (b <= 0) = return Nothing
    | (n == x) && (b == 1) =
--           trace "found" $
                 return $ Just [x]
    | otherwise =
           do
             without_x <- solve xs n b (k - 1)
             case without_x of
               Nothing -> do
                   rs <- solve xs (n - x) (b - 1) (k - 1)
                   return ((:) x <$> rs)
               Just rs -> return $ Just $  rs
    
main = do
  t <- read <$> getLine
  mapM_ (\_ -> do
           [n, k, b] <-  map read <$> words <$> getLine
           let rk = traceShowId (if n > k then k else n)
           let s = evalState (solve [1..rk] n b rk) M.empty
           putStrLn ((unwords $ map show (maybe [-1] id s)))
        ) [1..t]
