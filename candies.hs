import Debug.Trace
import Data.List 
import Data.Functor
import qualified Data.Map as M    
import qualified Data.Vector as V
import Control.Monad
import Control.Applicative    
import Control.Monad.State    


solve_for :: Int -> V.Vector Int -> State (M.Map Int Int) Int 
solve_for idx xs = do
  m <- get
  case M.lookup idx m of
    Nothing -> do
      r <- solve_for_ idx xs
      let nm = M.insert idx r m
      put nm
      return r    
    Just r -> return r
         

solve_for_ :: Int -> V.Vector Int -> State (M.Map Int Int) Int
solve_for_ idx xs
    | idx == 0 =
        let we = (V.!) xs idx
            ridx = idx + 1
            r = (V.!) xs ridx
            cr = compare r we
        in case cr of
             LT -> (+) 1 <$> solve_for ridx xs
             GT -> return 1
             EQ -> return 1
    | idx + 1 == V.length xs =
        let we = (V.!) xs idx
            lidx = idx - 1
            l = (V.!) xs lidx
        in if l < we
           then (+) 1 <$> solve_for lidx xs
           else return 1
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
        (LT, LT) -> (+) 1 <$> solve_for lidx xs
        (GT, GT) -> (+) 1 <$> solve_for ridx xs
        (GT, LT) -> return 1
        (LT, GT) -> (+) 1 <$> (max <$> (solve_for lidx xs) <*> (solve_for ridx xs))
        (EQ, EQ) -> return 1
        (EQ, GT) -> (+) 1 <$>  solve_for ridx xs
        (EQ, LT) -> return 1
        (GT, EQ) -> return 1
        (LT, EQ) -> (+) 1 <$> solve_for lidx xs

    
solve :: V.Vector Int -> Int
solve xs =
      (evalState (foldM (\acc i -> (+) acc <$> (solve_for i xs)) 0 [0..(V.length xs)-1]) M.empty) 


main = do
  n <- read <$> getLine
  xs <- mapM (\_ -> read <$> getLine) [1..n]
  putStrLn $ show $ solve (V.fromList xs)
