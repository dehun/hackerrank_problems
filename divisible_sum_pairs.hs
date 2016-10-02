import Data.Functor
import Data.List    

solve :: [Int] -> Int -> Int
solve (x:xs) k =
    let divisions = (map (\a -> (a + x) `mod` k) xs)
    in  (length $ filter (== 0) divisions) + (solve xs k)
solve [] _ = 0
    

main = do
  [n, k] <- map read <$> take 2 <$> words <$> getLine :: IO [Int]
  a <- map read <$> take n <$> words <$> getLine :: IO [Int]
  putStrLn $ show $ solve a k
