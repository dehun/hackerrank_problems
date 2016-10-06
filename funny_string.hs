import Data.Ord
import Data.Functor
import Data.Char
import Debug.Trace    

pairs :: [Char] -> [(Char, Char)]
pairs x = zip (init x) (tail x)

    
solve :: String -> String
solve s =
    let r = reverse s
        sr_pairs = (zip (pairs s) (pairs r)) :: [((Char, Char), (Char, Char))]
    in if all (\((s1, s2), (r1, r2)) ->
                   abs (ord s2 - ord s1) == abs (ord r2 - ord r1)
              ) sr_pairs
       then "Funny"
       else "Not Funny"

    
main = do
  t <- read <$> getLine
  cases <- mapM (\_ -> getLine) [1..t]
  mapM_ (\c -> putStrLn $ solve c) cases
