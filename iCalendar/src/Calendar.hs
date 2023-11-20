{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Calendar where

import           Data.Char         (isAlpha, isAlphaNum, isAscii, isControl,
                                    isSpace, isSymbol)
import           Data.Either       (partitionEithers)
import           Data.List
import           Data.Maybe        (fromJust, fromMaybe)
import           Data.Sequence     (Seq (Empty))
import           DateTime
import           Debug.Trace
import           ParseLib.Abstract
import           Prelude           hiding (sequence, ($>), (*>), (<$), (<*),
                                    (<>))


-- Exercise 6
data Calendar = Calendar {
  calProp :: [CalendarProp],
  events  :: [Event]
} deriving (Eq, Ord, Show)

data CalendarProp = Version String
                  | ProdId  String
                  deriving (Eq, Ord, Show)

data EventProp = DtStamp     DateTime
               | Uid         String
               | DtStart     DateTime
               | DtEnd       DateTime
               | Description String
               | Summary     String
               | Location    String
               deriving (Eq, Ord, Show)

data Event = Event { eventProps :: [EventProp] } deriving (Eq, Ord, Show)

data CalendarDay = EmptyDay
                 | CDay Day deriving (Eq, Ord, Show)
type CalendarDayBlock = (CalendarDay, [Event])

-- Exercise 7
data Token = Token Calendar
  deriving (Eq, Ord, Show)

-- Test data calendar
calendarString :: String
calendarString = "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:-//hacksw/handcal//NONSGML v1.0//EN\r\nBEGIN:VEVENT\r\nUID:12345@example.com\r\nDTSTAMP:20111205T170000Z\r\nDTSTART:20111205T170000Z\r\nDTEND:20111205T210000Z\r\nSUMMARY:This is a very long description th\r\n at spans over multiple lines.\r\nEND:VEVENT\r\nEND:VCALENDAR\r\n"

calendar :: Calendar
calendar = fromJust $ recognizeCalendar calendarString

readCalendar :: FilePath -> IO (Maybe Calendar)
readCalendar path = do
  string <- readFile path
  return $ recognizeCalendar string

calendarStaysSame :: Bool
calendarStaysSame = printCalendar calendar == calendarString

printCalendarTest :: IO ()
printCalendarTest = putStrLn $ printCalendar calendar

isNewLine :: Char -> Bool
isNewLine '\r' = True
isNewLine '\n' = True
isNewLine _    = False

newLines :: Parser Char String
newLines = greedy (satisfy isNewLine)

continueNewLine :: Parser Char String
continueNewLine = newLines <* symbol ' '

identifier' :: Parser Char String
identifier' = greedy (satisfy (\c -> isAscii c && not (isNewLine c)))

parseValue :: Parser Char [String]
parseValue = (:) <$> identifier' <* newLines <*> parseValue'
  where
    parseValue' = greedy (symbol ' ' *> identifier' <* newLines)

-- Calendar
parseVersion :: Parser Char CalendarProp
parseVersion = Version . concat <$ token "VERSION:" <*> parseValue

parseProdId :: Parser Char CalendarProp
parseProdId = ProdId . concat <$ token "PRODID:" <*> parseValue

parseCalendarProp :: Parser Char CalendarProp
parseCalendarProp = parseProdId <|> parseVersion

-- Event
parseDtStamp :: Parser Char EventProp
parseDtStamp = DtStamp <$ token "DTSTAMP:" <*> parseDateTime <* newLines

parseUid :: Parser Char EventProp
parseUid = Uid . concat <$ token "UID:" <*> parseValue

parseDtStart :: Parser Char EventProp
parseDtStart = DtStart <$ token "DTSTART:" <*> parseDateTime <* newLines

parseDtEnd :: Parser Char EventProp
parseDtEnd = DtEnd <$ token "DTEND:" <*> parseDateTime <* newLines

parseDescription :: Parser Char EventProp
parseDescription = Description . concat <$ token "DESCRIPTION:" <*> parseValue

parseSummary :: Parser Char EventProp
parseSummary = Summary . concat <$ token "SUMMARY:" <*> parseValue

parseLocation :: Parser Char EventProp
parseLocation = Location . concat <$ token "LOCATION:" <*> parseValue

parseEventProp :: Parser Char EventProp
parseEventProp = parseDtStamp
             <|> parseUid
             <|> parseDtStart
             <|> parseDtEnd
             <|> parseDescription
             <|> parseSummary
             <|> parseLocation

parseEvent :: Parser Char Event
parseEvent =
  Event
    <$ token "BEGIN:VEVENT" <* newLines
    <*> many parseEventProp
    <* token "END:VEVENT" <* newLines

tCalendar :: Parser Char Token
tCalendar =
  Token
    <$> (Calendar
          <$ token "BEGIN:VCALENDAR" <* newLines
          <*> many parseCalendarProp
          <*> many parseEvent
          <* token "END:VCALENDAR" <* newLines)

scanCalendar :: Parser Char [Token]
scanCalendar = many tCalendar

fromCalendar :: Token -> Calendar
fromCalendar (Token c) = c

parseCalendar :: Parser Token Calendar
parseCalendar = fromCalendar <$> anySymbol

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run scanCalendar s >>= \g -> run parseCalendar g

-- Exercise 8
stringBreak :: String -> String
stringBreak s
  | not $ null drop' = take 42 s ++ "\r\n" ++ stringBreak (' ' : drop')
  | otherwise         = s
  where
    drop' = drop 42 s

printLines :: [a] -> (a -> String) -> String
printLines xs f = concatMap (\x -> stringBreak (f x) ++ "\r\n") xs

printEvent' :: EventProp -> String
printEvent' (Summary     s ) = "SUMMARY:" ++ s
printEvent' (Uid         s ) = "UID:" ++ s
printEvent' (DtStamp     dt) = "DTSTAMP:" ++ printDateTime dt
printEvent' (DtStart     dt) = "DTSTART:" ++ printDateTime dt
printEvent' (DtEnd       dt) = "DTEND:" ++ printDateTime dt
printEvent' (Description s ) = "DESCRIPTION:" ++ s
printEvent' (Location    s ) = "LOCATION:" ++ s

printEvent :: Event -> String
printEvent e = "BEGIN:VEVENT\r\n"
            ++ printLines (eventProps e) printEvent'
            ++ "END:VEVENT\r\n"

printCalendar' :: CalendarProp -> String
printCalendar' (ProdId  s) = "PRODID:" ++ s
printCalendar' (Version s) = "VERSION:" ++ s

printCalendar :: Calendar -> String
printCalendar c = "BEGIN:VCALENDAR\r\n"
               ++ printLines (calProp c) printCalendar'
               ++ concatMap printEvent (events c)
               ++ "END:VCALENDAR\r\n"
