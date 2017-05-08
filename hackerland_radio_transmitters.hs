import qualified Data.Vector as V
import Debug.Trace
import Data.Maybe
import Data.List    

indixate :: V.Vector Int -> V.Vector (Int, Int)    
indixate xs = V.zip (V.fromList [0..(V.length xs)])  xs
    
lowerBound :: Int -> V.Vector (Int, Int) -> Maybe Int
lowerBound k xs = fst <$> (lowerBoundHelper k xs)

lowerBoundHelper :: Int -> V.Vector (Int, Int) -> Maybe (Int, Int)
lowerBoundHelper k xs
    | V.length xs == 1 = if k < snd (V.head xs)
                            then Nothing
                            else Just (V.head xs)
    | V.length xs == 0 = Nothing
    | otherwise = 
    let (ls, rs) = V.splitAt (V.length xs `div` 2) xs
        mid = V.head rs
    in if (snd mid <= k)
       then lowerBoundHelper k rs
       else lowerBoundHelper k ls



strictUpperBound :: Int -> V.Vector (Int, Int) -> Maybe Int
strictUpperBound k xs =
    fst <$> strictUpperBoundHelper k xs

strictUpperBoundHelper :: Int -> V.Vector (Int, Int) -> Maybe (Int, Int)
strictUpperBoundHelper k xs 
    | V.length xs == 1=  if k >= (snd (V.head xs))
                         then Nothing
                         else Just $ V.head xs
    | otherwise = 
        let (ls, rs) = V.splitAt (length xs `div` 2) xs
            mid = snd $ V.head rs
        in if mid <= k
           then strictUpperBoundHelper k rs
           else case strictUpperBoundHelper k ls of
                  Nothing -> strictUpperBoundHelper k rs
                  x -> x

coverLeft :: Int -> V.Vector (Int, Int) -> V.Vector (Int, Int)
coverLeft k xs = let
    offset = fst $ V.head xs
    transmitterIdx = fromJust $ lowerBound (snd (V.head xs) + k) xs
    transmitter = snd (xs V.! (transmitterIdx - offset))
    rightEdge = strictUpperBound (transmitter + k)  xs
    in case rightEdge of
         Just re -> snd (V.splitAt (re - offset) xs)
         Nothing -> V.fromList []
    
    
solve :: Int -> [Int] -> Int
solve k xs =
    let nxs = indixate (V.fromList $ sort xs)
        covers = V.unfoldr (\rs ->
                                let nxt = coverLeft k rs
                                in if V.length nxt == 0
                                   then Nothing
                                   else Just (nxt, nxt)
                           ) nxs
    in 1 + V.length covers

main = do
  n : k : [] <- map read <$> words <$> getLine
  xs <- take n <$> map read <$> words <$> getLine :: IO [Int]
  putStrLn $ show $ solve k xs
