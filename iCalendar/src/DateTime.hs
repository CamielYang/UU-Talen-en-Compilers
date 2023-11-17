module DateTime where

import           Data.Char         (digitToInt)
import           ParseLib.Abstract
import           Prelude           hiding (sequence, ($>), (*>), (<$), (<*))

-- | "Target" datatype for the DateTime parser, i.e, the parser should produce elements of this type.
data DateTime = DateTime { date :: Date
                         , time :: Time
                         , utc  :: Bool }
    deriving (Eq, Ord, Show)

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
parseYear = Year <$> integer

parseMonth :: Parser Char Month
parseMonth = (\a b -> Month $ digitsToInt [a,b]) <$> integer <*> integer

parseDay :: Parser Char Day
parseDay = (\a b -> Day $ digitsToInt [a,b]) <$> integer <*> integer

parseHour :: Parser Char Hour
parseHour = (\a b -> Hour $ digitsToInt [a,b]) <$> integer <*> integer

parseMinute :: Parser Char Minute
parseMinute = (\a b -> Minute $ digitsToInt [a,b]) <$> integer <*> integer

parseSecond :: Parser Char Second
parseSecond = (\a b -> Second $ digitsToInt [a,b]) <$> integer <*> integer

parseDate :: Parser Char Date
parseDate = Date <$> parseYear <*> parseMonth <*> parseDay

parseTime :: Parser Char Time
parseTime = Time <$> parseHour <*> parseMinute <*> parseSecond

parseUTC :: Parser Char Bool
parseUTC = (True <$ symbol 'Z') <|> (False <$ epsilon)

parseDateTime :: Parser Char DateTime
parseDateTime = (\a _ c d -> DateTime a c d) <$>
                parseDate <*> symbol 'T' <*> parseTime <*> parseUTC

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
printDateTime (DateTime d t utc) = showTwoDigit (runYear $ year d)
                                ++ showTwoDigit (runMonth $ month d)
                                ++ showTwoDigit (runDay $ day d)
                                ++ "T"
                                ++ showTwoDigit (runHour $ hour t)
                                ++ showTwoDigit (runMinute $ minute t)
                                ++ showTwoDigit (runSecond $ second t)
                                ++ (if utc then "Z" else "")

-- printDateTime' :: DateTime -> String
-- printDateTime' (DateTime d t utc) = shows "hello" . shows "world" "ff"

-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime = undefined
