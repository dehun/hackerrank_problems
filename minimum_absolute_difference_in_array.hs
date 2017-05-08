import Data.Functor
import Data.List
import Debug.Trace

solve :: [Int] -> Int
solve xs =
    let nxs = sort xs
        pairs = zip (init nxs) (tail nxs)
        differences = map (\(l, r) -> abs (l - r)) pairs
    in minimum differences
    

main = do
  n <- read <$> getLine :: IO Int
  ns <- map read <$> (words <$> getLine)
  putStrLn $ show $ solve ns
