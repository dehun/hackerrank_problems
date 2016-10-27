import Data.Functor
import Debug.Trace
import qualified Data.Foldable as Foldable
import Data.Either    
import qualified Data.Sequence as S
import qualified Data.List as List    

data GameStep = GameStep {
      gs_id :: Integer
    , gs_m :: Integer
    , gs_w :: Integer
    , gs_n :: Integer
      } deriving (Eq, Show)

compare_steps :: GameStep -> GameStep -> Ordering
compare_steps lhs rhs =
    case ( compare (gs_m lhs) (gs_m rhs)
         , compare (gs_w lhs) (gs_w rhs)
         , compare (gs_n lhs) (gs_n rhs))
    of (LT, LT, LT) -> LT
       (GT, GT, GT) -> GT
       _ -> EQ


solve :: Integer -> Integer -> Integer -> Integer -> Integer
solve start_m start_w p n =
    let
        expand_game_step :: GameStep -> [GameStep]
        expand_game_step s =
            let
                this_step_n = gs_n s
                can_buy = this_step_n `div` p
                all_next_steps = [GameStep (1 + gs_id s)
                                           (nm + gs_m s)
                                           (nw + gs_w s)
                                           ((gs_n s) + ((gs_m s + nm) * (gs_w s + nw)) - p * (nm + nw))
                                 | nw <- [0..can_buy], nm <- [0..can_buy - nw]]
                next_valid_steps = all_next_steps
            in next_valid_steps
        bfs :: [GameStep] -> Integer
        bfs steps =
            let all_next_steps = Foldable.concatMap expand_game_step steps
                next_good_steps = all_next_steps
                is_reached_taget = Foldable.any (\s -> gs_n s >= n) steps
            in if is_reached_taget
               then gs_id $ head steps
               else bfs next_good_steps 

    in bfs [GameStep 1 start_m start_w (start_m * start_w)]
    
main = do
  [m, w, p, n] <- map read <$> words <$> getLine
  putStrLn $ show $ solve m w p n
