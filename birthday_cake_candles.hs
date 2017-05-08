import Data.Maybe
import Data.List    
import Data.Functor
import Control.Monad.State
import Control.Monad    

count_max :: [Int] -> Int
count_max (x:xs) =
    snd $ foldl cnt_max (x, 1) xs
    where cnt_max (old_candle, old_cnt) c
              | old_candle > c = (old_candle, old_cnt)
              | old_candle < c = (c, 1)
              | old_candle == c = (old_candle, old_cnt + 1)

    
main = do
  nc <- read <$> getLine :: IO Int
  candles <- map read <$> words <$> getLine :: IO [Int]
  putStrLn $ show $ count_max candles
