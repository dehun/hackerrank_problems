import Control.Monad.Writer
import Data.List    


innerSolve :: String -> Writer [String] ()
innerSolve s = do
    rest <- foldM (\acc c ->
                       case find (==c) ['A'..'Z'] of
                         Just _ -> do
                           tell $ [acc ++ [c]]
                           return []
                         Nothing -> do
                           return$  acc ++ [c]
                  ) "" s
    tell [rest]
    return ()
    
    
solve :: String -> Int
solve s =
    let ((), w) = runWriter (innerSolve s)
    in length $ filter (/=[]) w

      

main = do
  s <- getLine
  putStrLn $ show $ solve s
