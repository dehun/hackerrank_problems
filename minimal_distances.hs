import qualified Data.Map as M
import Data.List    
import Data.Functor
import Data.Maybe    

solve :: [Int] -> Int
solve xs =
    let (smallest, _, _) = foldl folder (Nothing, (M.empty :: (M.Map Int Int)), 0) xs
        folder :: (Maybe Int, M.Map Int Int, Int) -> Int -> (Maybe Int, M.Map Int Int, Int)
        folder (smallestSoFar, occurances, idx) x =
            case M.lookup x occurances of
              Nothing ->
                  (smallestSoFar, M.insert x idx occurances, idx + 1)
              Just pidx ->
                  ( Just $ maybe (idx - pidx) (\s -> min (idx - pidx) s) smallestSoFar
                  , M.insert x idx occurances
                  , idx + 1)
    in maybe (-1) id smallest
    

main = do
  n <- read <$> getLine
  xs <- map read <$> take n <$> words <$> getLine
  putStrLn $ show $ solve xs
