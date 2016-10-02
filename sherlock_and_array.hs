import qualified Data.Vector as V
import Data.Functor
import Data.Maybe

solve :: V.Vector Int -> Int -> Int -> Maybe Int
solve xs plsum prsum =  {-# SCC "mean" #-}
                        if V.length xs == 0
                        then Nothing
                        else solve_ xs plsum prsum                   
solve_ xs plsum prsum =
    let
        halfIndex = (V.length xs) `div` 2
        athalf = (V.!) xs halfIndex
        (left, right) = (\(l, r) -> (l, V.drop 1 r)) (V.splitAt halfIndex xs)
        lsum = plsum + V.sum left
        rsum = prsum + V.sum right
    in
      if V.length xs == 1
      then if plsum == prsum
           then Just 0
           else Nothing
      else if lsum == rsum
           then Just halfIndex
           else if lsum < rsum
                then
                    (+) halfIndex <$> solve right (lsum + athalf) prsum
                else
                    solve left plsum (rsum + athalf)

main = do
  t <- read <$> getLine
  cases <- mapM (\_ -> do
                     n <- read <$> getLine :: IO Int
                     (V.fromList <$>  map read <$> take n <$> words <$> getLine) :: IO (V.Vector Int)
                ) [1..t]
  mapM (\c -> putStrLn $ maybe "NO" (\_ -> "YES") (solve c 0 0)) cases
