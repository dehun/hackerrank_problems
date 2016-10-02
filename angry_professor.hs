import Data.Functor
import Control.Applicative

solve :: Int -> [Int] -> String
solve k ts =
    let present = length $ filter (\x -> x <= 0) ts in
    if present >= k
    then "NO"
    else "YES"
    
readCase = do
  [n, k] <- map read <$> words <$> getLine :: IO [Int]
  ts <- map read <$> take n <$> words <$> getLine :: IO [Int]
  return (k, ts)
    
main = do
  t <- read <$> getLine :: IO Int
  cases <- mapM (\_ -> readCase) [1..t]
  let solutions = map (uncurry solve) cases
  mapM putStrLn solutions
