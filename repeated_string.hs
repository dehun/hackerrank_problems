import Data.Functor

solve :: String -> Integer -> Integer
solve s n =
    let
        rest = n `mod` fromIntegral (length s)
        nfulls = n `div` fromIntegral (length s)
        as_in_full = fromIntegral $ length $ filter (=='a') s
        as_in_rest = fromIntegral $ length $ filter (=='a') $ take (fromIntegral rest) s
    in
      as_in_full * nfulls + as_in_rest

main = do
  s <- getLine
  n <- read <$> getLine :: IO Integer
  putStrLn $ show $ solve s n
