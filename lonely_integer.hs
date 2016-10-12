import Data.List
import Data.Functor
import qualified Data.Set as S    

solve :: [Int] -> Int
solve ys =
    let s = sort ys
        ff (x:y:xs) =
            if x /= y
            then x
            else ff xs
        ff (x:[]) = x                 
        ff [] = error "fck"

    in ff s

main = do
  n <- read <$> getLine
  xs <- map read <$> take n <$> words <$> getLine
  putStrLn $ show $ solve xs
