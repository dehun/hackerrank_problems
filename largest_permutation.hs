import Data.Functor
import Control.Monad    
import qualified Data.List as L    
import qualified Data.Vector as V    

    
solve :: V.Vector Int -> Int -> V.Vector Int
solve xs k =
    let 
    

show_vector :: V.Vector Int -> String
show_vector v = join (L.intersperse " " (map show $ V.toList v))

main = do
  t <- read <$> getLine
  mapM_ (\_ -> do
             [n, k] <- map read <$> words <$> getLine
             xs <- V.fromList <$> map read <$> take n <$> words <$> getLine
             putStrLn $ show_vector $ solve xs k
        )  [1..t]

