module DateTime where

import           Data.Char         (digitToInt)
import           ParseLib.Abstract
import           Prelude           hiding (sequence, ($>), (*>), (<$), (<*))

-- | "Target" datatype for the DateTime parser, i.e, the parser should produce elements of this type.
data DateTime = DateTime { date :: Date
                         , time :: Time
                         , utc  :: Bool }
    deriving (Eq, Ord)

instance Show DateTime where
    show = printDateTime

data Date = Date { year  :: Year
                 , month :: Month
                 , day   :: Day }
    deriving (Eq, Ord, Show)

newtype Year  = Year  { runYear  :: Int } deriving (Eq, Ord, Show)
newtype Month = Month { runMonth :: Int } deriving (Eq, Ord, Show)
newtype Day   = Day   { runDay   :: Int } deriving (Eq, Ord, Show)

data Time = Time { hour   :: Hour
                 , minute :: Minute
                 , second :: Second }
    deriving (Eq, Ord, Show)

newtype Hour   = Hour   { runHour   :: Int } deriving (Eq, Ord, Show)
newtype Minute = Minute { runMinute :: Int } deriving (Eq, Ord, Show)
newtype Second = Second { runSecond :: Int } deriving (Eq, Ord, Show)

-- Exercise 1
digitsToInt :: [Int] -> Int
digitsToInt = foldl (\acc x -> acc * 10 + x) 0

parseYear :: Parser Char Year
parseYear = (\a b c d -> Year $ digitsToInt [a,b,c,d]) <$> newdigit <*> newdigit <*> newdigit <*> newdigit

parseMonth :: Parser Char Month
parseMonth = (\a b -> Month $ digitsToInt [a,b]) <$> newdigit <*> newdigit

parseDay :: Parser Char Day
parseDay = (\a b -> Day $ digitsToInt [a,b]) <$> newdigit <*> newdigit

parseHour :: Parser Char Hour
parseHour = (\a b -> Hour $ digitsToInt [a,b]) <$> newdigit <*> newdigit

parseMinute :: Parser Char Minute
parseMinute = (\a b -> Minute $ digitsToInt [a,b]) <$> newdigit <*> newdigit

parseSecond :: Parser Char Second
parseSecond = (\a b -> Second $ digitsToInt [a,b]) <$> newdigit <*> newdigit

parseDate :: Parser Char Date
parseDate = Date <$> parseYear <*> parseMonth <*> parseDay

parseTime :: Parser Char Time
parseTime = Time <$> parseHour <*> parseMinute <*> parseSecond

parseUTC :: Parser Char Bool
parseUTC = (True <$ symbol 'Z') <|> (False <$ epsilon)

parseDateTime :: Parser Char DateTime
parseDateTime = DateTime <$>
                parseDate <* symbol 'T' <*> parseTime <*> parseUTC

-- Exercise 2
run :: Parser a b -> [a] -> Maybe b
run p xs = case parse p xs of
            []           -> Nothing
            ((a, _) : _) -> Just a

-- Exercise 3

-- 20101231T230000 == Date: 2010 12 31 - Time: 23:00:00 - UTC: False
testDt :: DateTime
testDt = DateTime
          (Date (Year 2010) (Month 12) (Day 31))
          (Time (Hour 23) (Minute 0) (Second 0))
          False

showTwoDigit :: Show a => a -> String
showTwoDigit s
  | length digit == 1 = '0' : digit
  | otherwise         = digit
  where
    digit = show s

printDateTime :: DateTime -> String
printDateTime (DateTime d t utc) =
  concat [showTwoDigit (runYear $ year d)
        , showTwoDigit (runMonth $ month d)
        , showTwoDigit (runDay $ day d)
        , "T"
        , showTwoDigit (runHour $ hour t)
        , showTwoDigit (runMinute $ minute t)
        , showTwoDigit (runSecond $ second t)
        , if utc then "Z" else ""]

checkMonthDay :: Year -> Month -> Day -> Bool
checkMonthDay y (Month m) (Day d)
  | m == 2 && ((isLeapYear y && d <= 29) || d <= 28) = True
  | m `elem` [1, 3, 5, 7, 8, 10, 12] && d <= 31 = True
  | m `elem` [4, 6, 9, 11] && d <= 30 = True
  | otherwise = False

isLeapYear :: Year -> Bool
isLeapYear (Year year) = (year `mod` 4 == 0) && ((year `mod` 400 == 0) || (mod year 100 /= 0))

-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime (DateTime d t utc) =
  year'   >= 1 &&
  day'    >= 1 &&
  hour'   >= 0 && hour'   <= 23 &&
  minute' >= 0 && minute' <= 59 &&
  second' >= 0 && second' <= 59 &&
  checkMonthDay (year d) (month d) (day d)
  where
    year'   = runYear   $ year   d
    month'  = runMonth  $ month  d
    day'    = runDay    $ day    d
    hour'   = runHour   $ hour   t
    minute' = runMinute $ minute t
    second' = runSecond $ second t

-- Exercise 10
getDays :: Year -> Month -> Int
getDays y (Month m)
  | m == 2 && isLeapYear y = 29
  | m == 2 = 28
  | m `elem` [1, 3, 5, 7, 8, 10, 12] = 31
  | m `elem` [4, 6, 9, 11] = 30
  | otherwise = 0

humanReadableTime :: DateTime -> String
humanReadableTime (DateTime _ (Time (Hour h) (Minute m) _) _) =
  showTwoDigit h ++ ":" ++ showTwoDigit m
