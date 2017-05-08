import Data.List
import Data.Functor

data AmPm = AM | PM deriving (Show, Eq)
instance Read AmPm where
    readsPrec _ input =
        let (ampm, rest) = splitAt 2 input
        in case ampm of
          "AM" -> [(AM, rest)]
          "PM" -> [(PM, rest)]

data AmPmTime = AmPmTime {hours::Int, minutes::Int, seconds::Int, ampm::AmPm} deriving (Show, Eq)
data MilitaryTime = MilitaryTime {mil_hours::Int, mil_minutes::Int, mil_seconds::Int} deriving (Eq)
instance Show MilitaryTime where
    show (MilitaryTime h m s) =
        let show_digits x 
                | x < 10 = "0" ++ show x
                | otherwise = show x
        in show_digits h ++ ":" ++ show_digits m ++ ":" ++ show_digits s

convert_ampm_to_military_time :: AmPmTime -> MilitaryTime
convert_ampm_to_military_time (AmPmTime 12 m s AM) = MilitaryTime 00 m s
convert_ampm_to_military_time (AmPmTime 12 m s PM) = MilitaryTime 12 m s
convert_ampm_to_military_time (AmPmTime h m s ampm) =
    let additor = if ampm == AM then 0 else 12
    in MilitaryTime (h + additor) m s 
    

wordsBy :: String -> Char -> [String]    
wordsBy xs sep =
    let indices =  findIndices (==sep) xs
        indices_pairs = (-1, head indices) :  zip (init indices) (tail indices) ++ [(last indices, length xs)]
        parts = map (\(l, r) -> (l + 1, r - 1 )) indices_pairs 
        takepart (l,r) = take (r - l + 1) (drop l xs)
    in map takepart parts

parse_ampmtime :: String -> AmPmTime
parse_ampmtime line =
    let h:m:sampm:[] = wordsBy line ':'
        (s, ampm) = splitAt 2 sampm
    in AmPmTime (read h) (read m) (read s) (read ampm)



main = do
  time <- parse_ampmtime <$> getLine
  putStrLn $ show $ convert_ampm_to_military_time time
  
