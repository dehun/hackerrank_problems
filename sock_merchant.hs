import Data.Functor
import Data.List    

solve :: [Int] -> Int
solve socks =
    sum (map (\x -> length x `div` 2) (group $ sort $ socks))
    
main = do
  n <- read <$> getLine :: IO Int
  socks <- map read <$> take n <$> words <$>getLine :: IO [Int]
  putStrLn $ show $ solve socks
