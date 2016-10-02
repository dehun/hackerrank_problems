import Control.Applicative

solve :: Integer -> Integer -> Integer -> Integer    
solve n m s =
    let prisoner = (s + m - 1) `mod` n
    in if prisoner == 0
       then n
       else prisoner
    
main = do
  t <- (read :: String -> Integer) <$> getLine
  mapM (\n -> do
            l <- getLine
            let [n, m, s] = map (read :: String -> Integer ) (words l)
            putStrLn $ show $  solve n m s
       )
       [1..t]
