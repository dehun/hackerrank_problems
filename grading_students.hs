import Data.Functor

grade n
      | 5 - n `mod` 5 < 3 = let rounded = n + 5 - n `mod` 5 in
                            if rounded >= 40
                            then rounded
                            else n
      | n < 40 = n                                 
      | otherwise = n

main = do
  n <- read <$> getLine :: IO Int
  grades <- mapM (\_ -> read <$> getLine) [1..n]  :: IO [Int]
  let rounded = map grade grades
  mapM_ (putStrLn . show) rounded
