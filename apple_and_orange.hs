import Data.Functor
import Data.List

solve :: Int -> Int -> Int -> [Int] -> Int
solve s t a fs =
  let on_house f = and [a + f >= s, a + f <= t ] in
  length $ filter on_house fs

main = do
  [s, t] <- map read <$> take 2 <$> words <$> getLine
  [a, b] <- map read <$> take 2 <$> words <$> getLine
  [m, n] <- map read <$> take 2 <$> words <$> getLine
  apples <- map read <$> take m <$> words <$> getLine
  oranges <- map read <$> take n <$> words <$> getLine
  putStrLn $ show $ solve s t a apples
  putStrLn $ show $ solve s t b oranges
