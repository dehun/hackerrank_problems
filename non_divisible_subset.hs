import Debug.Trace
import Data.Functor
import Data.List    

solve :: Integer -> [Integer] -> Integer
solve k xs =
    fromIntegral $ (flip (-) 1) $ length $ traceShowId
                     $ foldl (\acc x ->
                                  if not $ any (\t -> (t + x) `mod` k == 0) acc
                                  then x : acc
                                  else acc
          ) [0] xs

main = do
  [n, k] <- map read <$> take 2 <$> words <$> getLine :: IO [Integer]
  s <- map read <$> take (fromIntegral n) <$> words <$> getLine :: IO [Integer]
  putStrLn $ show $ solve k s
