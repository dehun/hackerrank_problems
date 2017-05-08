import qualified Data.Set as S
import Data.List
import Data.Char

isPangram ::String -> Bool
isPangram s =
    let all_chars = S.fromList ['a' .. 'z']
        s_chars = S.map toLower (S.fromList s) 
    in  S.isSubsetOf all_chars s_chars

main = do
  s <- getLine
  if isPangram s 
  then putStrLn "pangram"
  else putStrLn "not pangram"
