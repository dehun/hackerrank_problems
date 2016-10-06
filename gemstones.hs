import Data.Functor
import qualified Data.Set as S

solve :: [String] -> Int
solve [] = 0
solve (x:xs) =
    S.size $ foldl (\acc g -> S.intersection acc (S.fromList g)) (S.fromList x) xs

    
main = do
  n <- read <$> getLine
  stones <- mapM (\_ -> getLine) [1..n]
  putStrLn $ show $ solve stones
