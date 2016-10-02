import Data.Functor
import Control.Monad

main = do
  let readTriplet = (map read <$> take 3 <$> words <$> getLine) :: IO [Integer]
  alice <-  readTriplet
  bob <- readTriplet
  score <- foldM (\(alice, bob) (a, b) -> if a < b
                                 then return (alice, bob + 1)
                                 else
                                     if a == b
                                     then return (alice, bob)
                                     else return (alice + 1, bob))
            (0, 0)
            (zip alice bob)
  putStrLn $ show (fst score) ++ " " ++ show (snd score)
