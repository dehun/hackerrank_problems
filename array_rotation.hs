import Data.Functor
import qualified Data.Vector as V

rrotate :: V.Vector Int -> Int -> V.Vector Int
rrotate xs k =
    let kk = k `mod` V.length xs
        (l, r) = V.splitAt ((V.length xs) - kk) xs
    in V.concat [r, l]
    
    

main = do
  [n, k, q] <- map read <$> words <$> getLine :: IO [Int]
  as <- V.fromList <$> (map read <$> words <$> getLine :: IO [Int])
  let rotated = rrotate as k
  mapM_ (\_ -> do
             m <- read <$> getLine
             putStrLn $ show $ ((V.!) rotated m)
        ) [1..q]
  return ()
