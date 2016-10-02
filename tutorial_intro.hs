import qualified Data.Vector as V
import Data.Functor    


bsearch :: Ord a => V.Vector a -> a -> Int
bsearch xs x =
    _bsearch 0 (V.length xs) 
     where _bsearch s e =
               let half = (s + e) `div` 2
                   at_half = xs V.! half
               in if at_half == x
                  then half
                  else if x < at_half
                       then _bsearch s half
                       else _bsearch half e

main = do
  x <- read <$> getLine :: IO Int
  n <- read <$> getLine :: IO Int
  xs <- V.map read <$> V.take n <$> V.fromList <$> words <$> getLine :: IO (V.Vector Int)
  putStrLn $ show $ bsearch xs x

