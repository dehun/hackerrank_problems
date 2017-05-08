import Data.List
import Data.Functor
import Debug.Trace

solve :: [Int] -> Int
solve xs =
    foldl (\s (j, c) ->  s + 2 ^ j * c) 0 (zip [0..] $ reverse $ sort xs)
main = do
  n <- read <$> getLine :: IO Int
  xs <- take n <$> map read <$> (words <$> getLine) :: IO [Int]
  putStrLn $ show $ solve xs
