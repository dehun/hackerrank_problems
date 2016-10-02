import Control.Applicative

solve :: Integer -> [Integer] -> Integer
solve k clouds =
    let
        traverseClouds energy cloud =
            if next_cloud == 0
            then new_energy
            else traverseClouds (new_energy - 1) next_cloud
            where cloud_energy = if clouds !! fromIntegral cloud == 0 then 0 else -2
                  new_energy = energy + cloud_energy
                  next_cloud = (cloud + k) `mod` (fromIntegral (length clouds))                         
    in (traverseClouds 100 0) - 1
                           
    
main = do
  [n, k] <- map read <$> words <$> getLine
  clouds <- map read <$> take (fromIntegral n) <$> words <$> getLine
  putStrLn $ show $ solve k clouds
