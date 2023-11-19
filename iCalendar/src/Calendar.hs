{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Calendar where

import           Data.Char         (isAlpha, isAlphaNum, isAscii, isControl,
                                    isSpace, isSymbol)
import           Data.Either       (partitionEithers)
import           DateTime
import           ParseLib.Abstract
import           Prelude           hiding (sequence, ($>), (*>), (<$), (<*))


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

-- Exercise 7
data Token = Token Calendar
  deriving (Eq, Ord, Show)

-- recognizeCalendar "BEGIN:VCALENDAR\r\nVERSION:w\r\nPRODID:w\r\nEND:VCALENDAR\r\n"
-- recognizeCalendar "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:-//hacksw/handcal//NONSGML v1.0//EN\r\nBEGIN:VEVENT\r\nUID:19970610T172345Z-AF23B2@example.com\r\nDTSTAMP:19970610T172345Z\r\nDTSTART:19970714T170000Z\r\nDTEND:19970715T040000Z\r\nSUMMARY:Bastille Day Party\r\nEND:VEVENT\r\nEND:VCALENDAR\r\n"

isNewLine :: Char -> Bool
isNewLine '\r' = True
isNewLine '\n' = True
isNewLine _    = False

newLines :: Parser Char String
newLines = greedy (satisfy isNewLine)

character :: Parser Char String
character = greedy (satisfy (\c -> isAscii c && not (isNewLine c)))

-- Calendar
parseVersion :: Parser Char CalendarProp
parseVersion = Version <$ token "VERSION:" <*> character <* newLines

parseProdId :: Parser Char CalendarProp
parseProdId = ProdId <$ token "PRODID:" <*> character <* newLines

parseCalendarProp :: Parser Char CalendarProp
parseCalendarProp = parseVersion <|> parseProdId

-- Event
parseDtStamp :: Parser Char EventProp
parseDtStamp = DtStamp <$ token "DTSTAMP:" <*> parseDateTime <* newLines

parseUid :: Parser Char EventProp
parseUid = Uid <$ token "UID:" <*> character <* newLines

parseDtStart :: Parser Char EventProp
parseDtStart = DtStart <$ token "DTSTART:" <*> parseDateTime <* newLines

parseDtEnd :: Parser Char EventProp
parseDtEnd = DtEnd <$ token "DTEND:" <*> parseDateTime <* newLines

parseDescription :: Parser Char EventProp
parseDescription = Description <$ token "DESCRIPTION:" <*> character <* newLines

parseSummary :: Parser Char EventProp
parseSummary = Summary <$ token "SUMMARY:" <*> character <* newLines

parseLocation :: Parser Char EventProp
parseLocation = Location <$ token "LOCATION:" <*> character <* newLines

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
  Token <$> (Calendar
              <$ token "BEGIN:VCALENDAR" <* newLines
              <*> many parseCalendarProp
              <*> many parseEvent
              <* token "END:VCALENDAR" <* newLines)

scanCalendar :: Parser Char [Token]
scanCalendar = many tCalendar

parseCalendar :: Parser Token Calendar
parseCalendar = fromCalendar <$> satisfy (const True)

fromCalendar :: Token -> Calendar
fromCalendar (Token c) = c

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run scanCalendar s >>= \g -> run parseCalendar g

-- Exercise 8
printEvent' :: EventProp -> String
printEvent' (Summary     s ) = "SUMMARY:" ++ s ++ "\r\n"
printEvent' (Uid         s ) = "UID:" ++ s ++ "\r\n"
printEvent' (DtStamp     dt) = "DTSTAMP:" ++ printDateTime dt ++ "\r\n"
printEvent' (DtStart     dt) = "DTSTART:" ++ printDateTime dt ++ "\r\n"
printEvent' (DtEnd       dt) = "DTEND:" ++ printDateTime dt ++ "\r\n"
printEvent' (Description s ) = "DESCRIPTION:" ++ s ++ "\r\n"
printEvent' (Location    s ) = "LOCATION:" ++ s ++ "\r\n"

printEvent :: Event -> String
printEvent e = "BEGIN:VEVENT\r\n"
            ++ concatMap printEvent' (eventProps e)
            ++ "END:VEVENT\r\n"

printCalendar' :: CalendarProp -> String
printCalendar' (ProdId  s) = "PRODID:" ++ s ++ "\r\n"
printCalendar' (Version s) = "VERSION:" ++ s ++ "\r\n"

printCalendar :: Calendar -> String
printCalendar c = "BEGIN:VCALENDAR\r\n"
               ++ concatMap printCalendar' (calProp c)
               ++ concatMap printEvent (events c)
               ++ "END:VCALENDAR\r\n"

