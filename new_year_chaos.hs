import Control.Monad
import Data.Functor
import Data.Maybe    

bsolve_round :: [(Int, Int)] -> Maybe [(Int, Int)] 
bsolve_round (x:[]) = Just (x:[])
bsolve_round ((x, cx):(y, cy):r) =
    let xstay_choice = (:)(x, cx) <$> (bsolve_round ((y, cy):r))
        xswap_choice = (:) (y, cy) <$> (bsolve_round ((x, cx + 1):r))
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
        sorted = foldM (\inp _ ->
                            bsolve_round inp
                       ) zeroed [1..length xs]
    in sum <$> map snd <$> sorted
        
    
solve :: [Int] -> Maybe Int
solve xs =
    foldM (\acc (i, x) ->
               let d = x - i
               in if d > 2
               then Nothing
               else if d > 0
                    then Just (acc  + x - i)
                    else Just acc
          ) 0 (zip [1..length xs] xs)

main = do
  t <- read <$> getLine
  mapM (\_ -> do
          n <- read <$> getLine
          xs <- map read <$> take n <$> words <$> getLine :: IO [Int]
          putStrLn $ maybe "Too chaotic" show (bsolve xs)
       ) [1..t]
  
