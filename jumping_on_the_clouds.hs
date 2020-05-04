go :: [Int] -> Int -> Int
go [] cnt =  cnt
go (0 : []) cnt =  cnt
go (0:0:0:rst) cnt  = go (0:rst) $ cnt + 1
go (0:0:rst) cnt  = go (0:rst) $ cnt + 1
go (0:1:0:rst) cnt = go (0:rst) $ cnt + 1


solve :: [Int] -> Int
solve xs =  go xs 0 

main = do
  n <- read <$> getLine
  xs <- map read <$> take n  <$> words <$> getLine
  putStrLn $ show $ solve xs
