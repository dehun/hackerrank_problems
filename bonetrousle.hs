import Data.Functor
import Data.Maybe
import Data.List
import Control.Monad
import Debug.Trace    
    


solve :: [Int] -> Int -> Int -> Maybe [Int]
solve xs n b =
--    trace ("solving " ++ show xs ++ "; n=" ++ show n ++ "; b=" ++ show b) $ $ traceShowId $
          solve_ xs n b

solve_ :: [Int] -> Int -> Int -> Maybe [Int]
solve_ [] n b = if b == 0 && n == 0 then Just [] else Nothing
solve_ (x:xs) n b
       | (b <= 0) = Nothing
       | (n <= 0) = Nothing
       | (n == x) && (b == 1) = trace "found" $ Just [x]
       | otherwise =
    let cases = [((:) x <$> (solve xs (n - x) (b - 1)))
                , solve xs n b] :: [Maybe [Int]]
        first_success = join (find isJust cases) :: Maybe [Int]
    in first_success
      
    
main = do
  t <- read <$> getLine
  mapM_ (\_ -> do
           [n, k, b] <-  map read <$> words <$> getLine
           let s = (solve [1..k] n b) :: Maybe [Int]
           putStrLn $ unwords $ map show (maybe [-1] id s)
        ) [1..t]
