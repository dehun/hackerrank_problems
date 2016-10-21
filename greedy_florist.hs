import Data.Functor
import Data.List    

solve :: Int -> [Int] -> Int
solve k flowers =
    let sorted_flowers = reverse $ sort flowers
    in fst $ foldl
           (\(fs, i)  f ->
                (fs + f * (1 + i `div` k), i + 1)
           )
           (0, 0) sorted_flowers

main = do
  [n, k] <- map read <$> take 2 <$> words <$> getLine
  flowers <- map read <$> take n <$> words <$> getLine
  putStrLn $ show $ solve k flowers
