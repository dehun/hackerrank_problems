import Data.Functor
import Control.Applicative    
import Data.Maybe    
import qualified Data.Vector as V

bsearch :: Int -> Int -> Int -> V.Vector Int -> Maybe Int
bsearch lidx plsum prsum xs = bsearch_ lidx plsum prsum xs
    
bsearch_ :: Int -> Int -> Int -> V.Vector Int -> Maybe Int
bsearch_ lidx plsum prsum xs
    | V.length xs == 1 = if plsum == prsum + V.head xs
                       then return lidx
                       else Nothing
    | otherwise = 
    let
        half = (V.length xs) `div` 2
        (l, r) = V.splitAt half xs
        lsum = plsum + V.sum l
        rsum = prsum + V.sum r
    in
      if lsum == 0 && rsum == 0
      then Just lidx
      else
          if lsum == rsum
          then return (lidx + half)
          else if lsum < rsum
               then bsearch (lidx + half) lsum prsum r
               else bsearch lidx plsum rsum l
    
split :: V.Vector Int -> Maybe (V.Vector Int, V.Vector Int)
split xs =
    let idx = bsearch 0 0 0 xs
    in V.splitAt <$> idx <*> Just xs

    
solve :: V.Vector Int -> Int
solve xs
    | V.all (==0) xs = (V.length xs) - 1
    | V.length xs < 2 = 0
    | otherwise =
    case split xs of
      Just (l, r) ->
          1 + (max (solve l) (solve r))
      Nothing -> 0
    
main = do
  t <- read <$> getLine
  mapM (\_ -> do
          n <- read <$> getLine
          xs <- V.fromList <$> map read <$> take n <$> words <$> getLine
          putStrLn $ show $ solve xs
          ) [1..t]
