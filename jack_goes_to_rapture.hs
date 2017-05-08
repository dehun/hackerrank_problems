import Data.Maybe

solve :: Int -> Int -> [(Int, Int, Int)] -> Maybe Int
solve s e edges = undefined

main = do
  n : e : []  <- map read <$> words <$> getLine :: IO [Int]
  let parse_edge line =
          let (n1: n2: c: [])  = map read (words line) :: [Int]
          in (n1, n2, c)
  edges <- mapM (\_ -> parse_edge <$> getLine) [1..e]
  case solve 1 (n - 1) edges of
    Just x -> putStrLn $ show $ x
    Nothing -> putStrLn "NO PATH EXISTS"


