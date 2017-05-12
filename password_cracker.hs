import Data.List
import qualified Data.Map.Strict as M
import Data.Functor
import Control.Monad
import Data.Maybe
import Control.Monad.State
import Debug.Trace    

data Trie = TNode { t_c :: Char, t_subs :: M.Map Char [Trie] }
          | TRoot { t_subs :: M.Map Char [Trie] }
          | TLeaf { t_c :: Char} deriving (Show, Eq)

emptyTrie = TRoot M.empty
emptySubtrie c = TNode c M.empty

buildTrie :: [String] -> Trie
buildTrie xs = foldl (\trie x -> insertInTrie trie x) emptyTrie $ (nub $ sortBy (\s1 s2 -> compare (length s2) (length s1)) xs)

insertInTrie :: Trie -> String -> Trie
insertInTrie (TRoot subs) (c:[]) =
    case M.lookup c subs of
      Nothing -> TRoot (M.insert c [TLeaf c] subs)
      Just subtries -> TRoot (M.insert c (TLeaf c:subtries) subs)
insertInTrie (TRoot subs) (c:cs) =
    case M.lookup c subs of
      Nothing ->
          let nt = insertInTrie (emptySubtrie c) cs
          in TRoot (M.insert c [nt] subs)
      Just subtries -> TRoot (M.insert c (map (\t -> insertInTrie t cs) subtries) subs)
insertInTrie (TNode nc subs) (c:[]) =
    case M.lookup c subs of
      Nothing -> TNode nc (M.insert c [TLeaf c] subs)
      Just subtries -> TNode nc (M.insert c (TLeaf c:subtries) subs)
insertInTrie (TNode nc subs) (c:cs) =
    case M.lookup c subs of 
      Nothing -> let nt = insertInTrie (emptySubtrie c) cs
                 in TNode nc (M.insert c [nt] subs)
      Just subtries -> TNode nc (M.insert c (map (\t -> insertInTrie t cs) subtries) subs)
insertInTrie (TLeaf c)  _ = TLeaf c

subqueryTrie :: Trie -> String -> String -> [(String, String)]
subqueryTrie (TLeaf c) cs res = [(reverse res, cs)]
subqueryTrie _ [] res = []
subqueryTrie subtrie (c:cs) res =
    let matchingSubs = case M.lookup c (t_subs subtrie) of
                         Just subs -> subs
                         Nothing -> []
    in concatMap (\sub -> subqueryTrie sub cs (t_c sub : res)) matchingSubs

queryTrie :: Trie -> String -> [(String, String)]
queryTrie trie la = subqueryTrie trie la ""

type Memoized = M.Map String (Maybe [String])
                    
subsolve :: Trie -> String -> State  Memoized (Maybe [String]) 
subsolve trie ""  = return $ Just []
subsolve trie la  =
    let parses = (reverse $ queryTrie trie la) 
        subseqs  =  mapM (\(parse, rest) -> do
                            old <- get
                            case M.lookup rest old of
                              Just sol -> return sol
                              Nothing -> do
                                          subsolution <- subsolve trie rest
                                          modify (\m -> M.insert rest subsolution m)
                                          return $ (:) parse <$> subsolution
                         ) parses
    in do
      join <$> find isJust <$> subseqs

solve :: [String] -> String -> Maybe [String]
solve ps la =
    let trie = buildTrie ps
    in evalState (subsolve trie la) M.empty

main = do
  n <- read <$> getLine :: IO Int
  qs <- mapM (\_ -> do
                nw <- read <$> getLine
                ps <- take nw <$> words <$> getLine
                la <- getLine
                return (ps, la)) [1..n]
  mapM (\(ps, la) -> case solve ps la of
                       Nothing -> putStrLn "WRONG PASSWORD"
                       Just psseq -> putStrLn $ concat $ intersperse " " psseq) qs
