import Data.Functor
import Data.List

solve :: [Int] -> (Int, Int)
solve xs = 
  let s = sum xs
      mi = minimum xs
      ma = maximum xs
  in (s - ma, s - mi)

main = do
  xs <- map read <$> take 5 <$> words <$> getLine
  let s = solve xs
  putStrLn $ intercalate " " $ map show [fst s, snd s]
