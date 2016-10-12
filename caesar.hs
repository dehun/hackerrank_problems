import Data.Functor
import Data.Char
import qualified Data.Set as S    

caesar :: Int -> [Char] -> Int -> [Char]
caesar n s k = map (\c ->
                        let cc = ord c
                            rs = if isUpper c then 'A' else 'a'
                            re = if isUpper c then 'Z' else 'z'
                            d = 1 + ord re - ord rs
                            ccb = cc - ord rs
                            ce = ord rs + (ccb + k) `mod` d
                        in if isAlpha c
                           then chr ce
                           else c
                   ) s


main = do
  n <- read <$> getLine
  s <- getLine
  k <- read <$> getLine
  putStrLn $ caesar n s k
