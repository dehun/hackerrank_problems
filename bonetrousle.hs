import Data.Functor
import Data.Maybe
import Data.List
import Control.Monad
import Debug.Trace    
    


solve :: [Int] -> Int -> Int -> Int -> Maybe [Int]
solve xs k n b =
--    trace ("solving " ++ show xs ++ "; k=" ++ show k ++ "; n=" ++ show n ++ "; b=" ++ show b) $ --traceShowId $
          solve_ xs k n b

solve_ :: [Int] -> Int -> Int -> Int -> Maybe [Int]
solve_ [] k n b = if b == 0 && n == 0 then Just [] else Nothing
solve_ (x:xs) k n b
       | k < b = Nothing
       | (k) * (x + x + k) `div` 2 < n = Nothing
       | (b <= 0) = Nothing
       | (n <= 0) = Nothing
       | (n == x) && (b == 1) =
--           trace "found" $
                 Just [x]
       | otherwise =
    let cases = [((:) x <$> (solve xs (k - 1) (n - x) (b - 1)))
                , solve xs (k - 1) n b] :: [Maybe [Int]]
        first_success = join (find isJust cases) :: Maybe [Int]
    in first_success
      
    
main = do
  t <- read <$> getLine
  mapM_ (\_ -> do
           [n, k, b] <-  map read <$> words <$> getLine
           let s = (solve [1..k] k n b) :: Maybe [Int]
           putStrLn $ unwords $ map show (maybe [-1] id s)
        ) [1..t]
