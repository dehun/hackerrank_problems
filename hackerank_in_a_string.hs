import Data.List
import Data.Functor
import Debug.Trace    

contains_sub :: String -> String -> Bool
contains_sub _ [] = True
contains_sub [] _ = False
contains_sub (q:qs) (s:ss)
    | q == s = contains_sub qs ss
    | otherwise = contains_sub qs (s:ss)


solve :: String -> String
solve q = if contains_sub q "hackerrank"
          then "YES"
          else "NO"
    
main = do
  nq <- read <$> getLine :: IO Int
  qs <- (mapM (\_ -> getLine) [1..nq])
  mapM (\q -> putStrLn $ solve q) qs
