import Data.List
import Data.Maybe
import Data.Functor
import Debug.Trace    

data Track = Track { t_row ::Int, t_c1 :: Int, t_c2 ::Int } deriving Eq
instance Show Track where
    show (Track r c1 c2) = show (r, c1, c2)

trackWidth :: Track -> Int
trackWidth (Track _ c1 c2) = c2 - c1 + 1

isOverlappingTrack :: Track -> Track -> Bool
isOverlappingTrack t1 t2 = and [ t_row t1 == t_row t2,
                                 t_c2 t1 >= t_c1 t2
                               ]

tracksOrder :: Track -> Track -> Ordering
tracksOrder t1 t2 = compare
                    [t_row t1, t_c1 t1, t_c2 t1]
                    [t_row t2, t_c1 t2, t_c2 t2]

sortTracks :: [Track] -> [Track]
sortTracks = sortBy tracksOrder


tracksByRow :: [Track] -> [[Track]]
tracksByRow = groupBy (\t1 t2 -> t_row t1 == t_row t2)

mergeTracks :: Track -> Track -> Either Track (Track, Track)
mergeTracks tl tr = if isOverlappingTrack tl tr
                    then Left 
                            $ Track (t_row tl) (min (t_c1 tl) (t_c1 tr)) (max (t_c2 tl) (t_c2 tr))
                    else Right (tl, tr)

squashTrack :: [Track] -> Track -> [Track]
squashTrack [] tp = [tp]
squashTrack (th:ts) tp = case mergeTracks th tp of
                           Left nth -> nth:ts
                           Right (nth1, nth2) -> nth1:nth2:ts

squashTracks :: [Track] -> [Track]
squashTracks ts = foldl squashTrack [] ts

solve :: Int -> Int -> [Track] -> Int
solve m n ts =
    let total = m * n
        squashedTracks = concatMap squashTracks 
                         $ tracksByRow
                         $  sortTracks ts
        trackWidths = map trackWidth squashedTracks
        filled = sum trackWidths
    in total - filled


main = do
    (m : n : t : []) <- map read <$> (words <$> getLine) :: IO [Int]
    ts <- mapM (\_ -> do
                    (r : c1 : c2: []) <- map read <$> words <$> getLine :: IO [Int]
                    return $ Track r c1 c2
               ) [1..t]
    putStrLn $ show $ solve m n ts
          
  
