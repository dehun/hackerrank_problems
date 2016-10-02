import Data.Functor
import Control.Monad.Writer
import Control.Monad
import Data.Monoid

solve :: String -> Integer
solve s = getSum $ snd $  (runWriter inner_solve)
          where
            ideal = take (length s) $ concat $ repeat "SOS"
            inner_solve :: Writer (Sum Integer) ()
            inner_solve =
                mapM_ (\(l, r) ->
                           if l /= r
                           then
                               do
                                 tell 1
                                 return ()
                           else
                               return ()) (zip s ideal)


main = do
  s <- getLine
  putStrLn $ show $ solve s
