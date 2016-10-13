import Data.Functor

lefraction :: [Int] -> Int -> (Int -> Bool) -> Float
lefraction xs n pred = fromIntegral (length (filter pred xs)) / (fromIntegral n)
    
main = do
  n <- read <$> getLine
  xs <- map read <$> take n <$> words <$> getLine
  putStrLn $ show $ lefraction xs n (\x -> x > 0)
  putStrLn $ show $ lefraction xs n (\x -> x < 0)
  putStrLn $ show $ lefraction xs n (\x -> x == 0)                      
