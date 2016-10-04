import Control.Monad
import Data.Functor
import Data.Maybe


bsolve_round :: Int -> [(Int, Int)] -> Maybe (Int, [(Int, Int)])
bsolve_round swaps (x:[]) = Just (swaps, x:[])
bsolve_round swaps ((x, cx):(y, cy):r) =
    let xstay_choice = (\(nswaps, narr) -> (nswaps, (:) (x, cx) narr)) <$> (bsolve_round swaps ((y, cy):r))
        xswap_choice = (\(nswaps, narr) -> (nswaps, (:) (y, cy) narr)) <$> (bsolve_round (swaps + 1) ((x, cx + 1):r))
    in
      if x > y
      then
          if cx + 1 > 2
          then Nothing
          else xswap_choice
      else xstay_choice


bsolve :: [Int] -> Maybe Int
bsolve xs =
    let zeroed = zip xs (take (length xs) (repeat 0))
        sorted = foldM (\(inp, old_swaps, skip) _ -> 
                          if skip
                          then return (inp, old_swaps, skip)
                          else do
                            (r, ninp) <- bsolve_round 0 inp
                            return (ninp, old_swaps + r, r == 0)
                       ) (zeroed, 0, False) [1..length xs]
    in (\(_, res, _) -> res) <$> sorted
        
    
main = do
  t <- read <$> getLine
  mapM (\_ -> do
          n <- read <$> getLine
          xs <- map read <$> take n <$> words <$> getLine :: IO [Int]
          putStrLn $ maybe "Too chaotic" show (bsolve xs)
       ) [1..t]
  
