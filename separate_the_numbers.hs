import Data.List

isBeautiful :: String -> Maybe Int
isBeautiful s =
    let splits = concatMap

solve s =
    case isBeautiful s of
      Nothing -> "NO"
      Just x -> "YES " ++ show x
                
main = do
  n <- read <$> getLine
  nums <- mapM (\_ -> read <$> getLine) [1..n]
  mapM (putStrLn <$> solve) nums 
