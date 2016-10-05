import Debug.Trace
import Data.List 
import Data.Functor
import qualified Data.Vector as V
import Control.Monad  


solve_for :: Int -> V.Vector Int -> Int
solve_for idx xs =
    -- let solutions = map (\i -> solve_for_ i xs)  [0..V.length xs - 1]
    -- in solutions !! idx
    solve_for_ idx xs


solve_for_ :: Int -> V.Vector Int -> Int
solve_for_ idx xs
    | idx == 0 =
        let we = (V.!) xs idx
            ridx = idx + 1
            r = (V.!) xs ridx
            cr = compare r we
        in case cr of
             LT -> 1 + solve_for ridx xs
             GT -> 1
             EQ -> 1
    | idx + 1 == V.length xs =
        let we = (V.!) xs idx
            lidx = idx - 1
            l = (V.!) xs lidx
        in if l < we
           then 1 + solve_for lidx xs
           else 1
    | otherwise =
    let we = (V.!) xs idx
        lidx = idx - 1
        ridx = idx + 1
        l = (V.!) xs lidx
        r = (V.!) xs ridx
        cl = compare l we
        cr = compare we r
    in
      case (cl, cr) of
        (LT, LT) -> 1 + solve_for lidx xs
        (GT, GT) -> 1 + solve_for ridx xs
        (GT, LT) -> 1
        (LT, GT) -> 1 + max (solve_for lidx xs) (solve_for ridx xs)
        (EQ, EQ) -> 1
        (EQ, GT) -> 1 + solve_for ridx xs
        (EQ, LT) -> 1
        (GT, EQ) -> 1
        (LT, EQ) -> 1 + solve_for lidx xs

    
solve :: V.Vector Int -> Int
solve xs =
    sum $ map (\i -> solve_for i xs) [0..(V.length xs)-1]


main = do
  n <- read <$> getLine
  xs <- mapM (\_ -> read <$> getLine) [1..n]
  putStrLn $ show $ solve (V.fromList xs)
