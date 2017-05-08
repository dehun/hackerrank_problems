import qualified Data.Vector as V
import qualified Data.List as L    
import Control.Monad.Writer    

type Matrix a = V.Vector (V.Vector a)

ringify :: Matrix a -> V.Vector (V.Vector a)

rotate_matrix :: Matrix Int -> Int -> Matrix Int
rotate_matrix = flip $ const $ id

show_matrix :: Matrix Int -> String
show_matrix rows =
    snd $ runWriter (V.mapM (\r -> do
                                tell "1" --(join $ L.intersperse "," (V.toList r))
                                return ()) rows)
                    

main = do
  [m, n, r] <- map read <$> words <$> getLine
  rows <- V.mapM (\_ -> V.fromList <$> map read <$> words <$> getLine) (V.fromList [1..m])
  putStr $ show_matrix $ rotate_matrix rows r
    
