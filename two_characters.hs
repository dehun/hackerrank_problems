import Data.List
import Debug.Trace
import Data.Ord    
import qualified Data.Set as S
import qualified Data.Map as M

merge (l:ls) (r:rs) = l:r:(merge ls rs)
merge [] (r:rs) = r:merge [] rs
merge (l:ls) [] = l:merge ls []
merge [] [] = []
    
spersed :: [Int] -> [Int] -> Bool
spersed ls rs =
    let merged = traceShowId $ merge ls rs
    in and [ all (\(l, r) -> l > r) (zip (init merged) (tail merged))
           , or [length ls == length rs, length ls == 1 + length rs, 1 + length ls == length rs] ]
    
isValidPair :: ([Int], [Int]) -> Bool
isValidPair (ls, rs) =
    or [ spersed ls rs
       , spersed rs ls]


solve :: String -> Int
solve s =
    let occurences = foldl (\acc (ix, c) -> case M.lookup c acc of
                                              Just ps -> M.insert c (ix:ps) acc
                                              Nothing -> M.insert c [ix] acc) M.empty (zip [1..] s)
        seqs = reverse $ sortBy (comparing length) (M.elems occurences)
        seqpairs = zip (init seqs) (tail seqs)
    in case find isValidPair seqpairs of
         Just p -> (length $ fst $ p) + (length $ snd $ p)
         Nothing -> 0


main = do
  n <- read <$> getLine
  s <- take n <$> getLine
  putStrLn $ show $ solve s
    
