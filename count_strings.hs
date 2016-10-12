import Data.Maybe
import Data.Functor
import Debug.Trace    
import Control.Monad
import Control.Monad.State    


data RToken = RA
            | RB
            | ROp RToken RToken RToken
            | RAnd --RToken RToken
            | ROr --RToken RToken 
            | RStar
            | RStart
            | REnd
              deriving (Eq, Show)


get_char :: State [Char] (Maybe Char)
get_char = do
    rs <- get
    case rs of
      (x:xs) -> do
              put xs
              return $ Just x
      [] -> return Nothing

lookahead_char :: State [Char] (Maybe Char)      
lookahead_char = do
  rs <- get
  case rs of        
   (x:xs) -> return $ Just x
   [] -> return Nothing
        
                       

-- (aa)
parse :: State [Char] RToken 
parse = do
  let parse_token c = do
          case c of
            Just 'a' -> return RA
            Just 'b' -> return RB
            Just '|' -> return ROr
            Just '(' -> return RStart
            Just ')' -> return REnd
            Just '*' -> return RStar
            Nothing -> return REnd

  Just RStart <- parse_token <$> get_char
  ll <- parse_token <$> lookahead_char
  l <- if ll == Just RStart
       then parse
       else do
         get_char
         return $ fromJust ll

  oop <- parse_token <$> lookahead_char
  op <- case oop of
        Just RStart -> return RAnd
        Just RAnd -> do
                   get_char
                   return RAnd
        Just ROr -> do
                   get_char
                   return ROr
        Just RStar -> return RStar
        _ -> return RAnd

  rr <- parse_token <$> lookahead_char
  r <- if rr == Just RStart
       then parse
       else do
         get_char
         return (fromJust rr)
  Just REnd <- parse_token <$> get_char

  return $ ROp l op r
  


count :: Integer -> RToken -> (Integer, [Integer])
count 1 RA = (1, [1])
count _ RA = (0, [])
count 1 RB = (1, [1])
count _ RB = (0, [])
count n (ROp l ROr r) =
    (\(lc, lv) (rc, rv) -> (lc + rc, lv ++ rv)) (count n l) (count n r)
count n (ROp l RAnd r) = undefined
count n (ROp l RStar RStar) = undefined
         
solve :: String -> Integer -> Integer
solve s n =
    let parsed = evalState parse s
        counted = count n parsed
    in fst counted
    

main = do
  t <- read <$> getLine
  mapM_ (\_ -> do
           [s, n] <- words <$> getLine
           let nn = read n :: Integer
           putStrLn $ show $ solve s nn
        ) [1..t]
