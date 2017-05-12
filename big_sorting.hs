import Data.List
import Data.Ord
import Data.Maybe
import Data.Functor

intRepr :: String -> String
intRepr xs = dropWhile (=='0') xs

compareStrs :: String -> String -> Ordering
compareStrs l r =
    let lip = intRepr l
        rip = intRepr r
    in compare (length lip,lip) (length rip, rip)

    
solve :: [String] -> [String]
solve xs = sortBy compareStrs xs

main = do
  n <- read <$> getLine
  xs <- mapM (\_ -> getLine) [1..n]
  let sorted_xs = solve xs
  mapM_ (\x -> putStrLn $ x) sorted_xs
