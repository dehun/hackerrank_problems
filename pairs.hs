import Data.List
import Data.Functor
import qualified Data.Vector as V

find_pairs k x ys = find_pairs_ k x ys
    
find_pairs_ :: Int -> Int -> V.Vector Int -> Int
find_pairs_ k x ys 
    | V.length ys < 1 = 0
    | V.length ys == 1 =
        let y = V.head ys
        in if x - y == k then 1 else 0
    | otherwise =
    let half = (V.length ys) `div` 2
        atfirst = V.head ys 
        athalf = (V.!) ys half
        (left, right) = V.splitAt half ys
        lpairs = V.takeWhile (\y -> x - y == k) ys
    in                 
      if x - atfirst == k
      then V.length lpairs
      else if x - athalf < k
           then find_pairs k x left
           else find_pairs k x right
    
    
isolve :: Int -> V.Vector Int -> V.Vector Int -> Int
isolve k arr rarr
    | V.length arr < 2 = 0
    | otherwise =
        let
            x = V.head arr
            xs = V.tail arr
            rx = V.last rarr
            rxs = V.init rarr 
            rest = isolve k xs rxs
            pairs = (find_pairs k x xs) + (find_pairs k rx rxs)
        in
          pairs +  rest


solve :: Int -> [Int] -> Int
solve k xs =
    let
        arr = V.fromList $ sort xs
        rarr = V.fromList $ sort xs
    in isolve k arr rarr

    
main = do
  [n, k] <- map read <$> words <$> getLine :: IO [Int]
  xs <- map read <$> words <$> getLine :: IO [Int]
  putStrLn $ show $ solve k xs
