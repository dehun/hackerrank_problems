import Data.Functor
import Debug.Trace
import qualified Data.Sequence as S

data GameStep = GameStep {
      gs_id :: Integer
    , gs_m :: Integer
    , gs_w :: Integer
    , gs_n :: Integer
      } deriving (Eq, Show)


solve :: Integer -> Integer -> Integer -> Integer -> Integer
solve start_m start_w p n =
    let
        bfs :: [GameStep] -> Integer
        bfs (h:queue) =
            let
                this_step_n = (gs_n h) + (gs_w h) * (gs_m h)
                can_buy = this_step_n `div` p
                all_next_steps = [GameStep (1 + gs_id h) (nm + gs_m h)
                                               (nw + gs_w h) (this_step_n - p * (nm + nw))
                                 | nw <- [0..can_buy], nm <- [0..can_buy - nw]]
                next_valid_steps = all_next_steps
            in if gs_n h >= n
               then gs_id h
               else bfs (queue ++ next_valid_steps)
    in bfs [GameStep 1 start_m start_w (start_m * start_w)]
    
main = do
  [m, w, p, n] <- map read <$> words <$> getLine
  putStrLn $ show $ solve m w p n
