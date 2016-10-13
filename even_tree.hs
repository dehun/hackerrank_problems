import Data.Functor
import Control.Applicative    
import Data.Maybe
import Debug.Trace    
import qualified Data.Set as S
import qualified Data.Map as M


data TNode = TNode { tn_id :: Int
                   , tn_subnodes :: [TNode] }
             deriving (Show, Eq)


tn_build :: Int -> [(Int, Int)] -> TNode
tn_build n edges =
    let joints = foldl (\acc (ll, rr) ->
                            let ins a l r = 
                                    case M.lookup l a of
                                      Nothing -> M.insert l [r] a
                                      Just redges -> M.insert l (r:redges) a
                            in ins (ins acc ll rr) rr ll)
                        M.empty edges
        subbuild_node nid =
            TNode nid (map subbuild_node (filter (\nnid -> nnid > nid) (fromJust $ M.lookup nid joints)))
    in subbuild_node 1


opt_balance :: Bool -> [(Maybe Int, Maybe Int)] -> Maybe Int
opt_balance e [] = if e then Just 0 else Nothing               
opt_balance e (x:xs) =
    let cases = [ (+) <$> fst x <*> (opt_balance (not e) xs)
                , (+) <$> snd x <*> opt_balance e xs ]
        succ_cases = map fromJust (filter isJust cases)
    in if succ_cases == []
       then Nothing
       else Just $ maximum succ_cases


solve :: TNode -> Int
solve tn =
    let solve_for :: TNode -> Bool -> Maybe Int
        solve_for tn p =
            let
                subsolutions = map (\sn -> ( solve_for sn True
                                           , (+) 1 <$>  solve_for sn False))
                               (tn_subnodes tn)
            in opt_balance p subsolutions
    in fromJust $ solve_for tn False
    
main = do
  [n, m] <- map read <$> words <$> getLine :: IO [Int]
  edges <- mapM (\_ -> (\[l, r] -> (l, r)) <$> map read <$> words  <$> getLine) [1..m] :: IO [(Int, Int)]
  putStrLn $ show $ solve $ tn_build n edges

  
