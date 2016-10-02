import Data.List
import Data.Maybe
import Data.Functor
import qualified Data.Vector as V

pparts :: V.Vector Char -> (V.Vector Char, V.Vector Char)
pparts s =
    let oddish = V.length s `mod` 2 == 0
        half = V.length s `div` 2
        lhalf = V.slice 0 half s
        rhalf = V.slice (half) ((V.length s) - half) s
    in if oddish
       then (lhalf, V.reverse rhalf)
       else (lhalf, V.reverse $ V.tail rhalf)

            
diff l r =
    foldl (\acc (x, y) -> if x /= y then acc + 1 else acc) 0 (zip (V.toList l) (V.toList r))

          
chop xs = if V.length xs <= 1
          then V.empty
          else V.tail $ V.init $ xs

-- solve k digits acc = trace ("call" ++ show (k, digits, acc)) $ traceShowId $ solve_ k digits acc
          
solve :: Integer -> Integer -> V.Vector Char -> Maybe String
solve d k digits
    | k < 0 = Nothing
    | (V.length digits) == 1 = if k > 0 then Just "9" else Just (V.toList digits)
    | k >= 0 && digits == V.empty = Just []
    | otherwise =
        let (l, r) = pparts digits
            lh = V.head l
            rh = V.head r
            non_changed_case = if lh == rh
                               then ((read [lh] :: Integer, k), solve (d - 1) k (chop digits))
                               else ((read [lh] :: Integer, k), Nothing)
            left_changed_case = ((read [rh] ::Integer, k - 1), solve (d-1) (k - 1) (chop digits))
            right_changed_case = ((read [lh] :: Integer, k - 1), solve (d-1) (k - 1) (chop digits))
            both_changed_case = ((9, k - 2), solve (d-1) (k - 2) (chop digits))
            search_order = (sortBy
                            (\(a, _) (b, _) -> compare b a)
                            [non_changed_case, left_changed_case, right_changed_case, both_changed_case])
        in
          if d > k
          then Nothing
          else
              case find (\c -> (snd c) /= Nothing)  search_order of
                Nothing -> Nothing
                Just ((ch, _), res) -> (:) (head $ show $ ch) <$> res
              

main = do
  [n, k] <- (map read <$> take 2 <$> words <$> getLine) :: IO [Integer]
  digits <- take (fromIntegral n) <$> getLine
  let half = (solve ((uncurry diff) (pparts $ V.fromList digits)) k (V.fromList digits))
  if n <= k
  then
      putStrLn $ take (fromIntegral n) $ repeat '9'
  else
      if half == Nothing
      then putStrLn $ "-1"
      else if(length digits) `mod` 2 == 0
           then putStrLn $ (fromJust half) ++ reverse (fromJust half)
           else putStrLn $ (fromJust half) ++ (reverse $ init $ (fromJust half))
