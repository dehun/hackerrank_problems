import Data.Functor
import Data.List    


solve :: Int -> [Int] -> Int
solve k ps =
    let ss = sort ps
        ff c l [] = c
        ff c l (x:xs) =
            if l > x
            then ff (c + 1) (l-x) xs
            else c
    in ff 0 k ss


main = do
  [n, k] <- map read <$> take 2 <$> words <$> getLine
  ps <- map read <$> words <$> getLine
  putStrLn $ show $ solve k ps
