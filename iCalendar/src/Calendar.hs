{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use void" #-}
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
data DtStamp = DtStamp { runDtStamp :: DateTime } deriving (Eq, Ord, Show)
data Uid     = Uid     { runUid     :: String   } deriving (Eq, Ord, Show)
data DtStart = DtStart { runDtStart :: DateTime } deriving (Eq, Ord, Show)
data DtEnd   = DtEnd   { runDtEnd   :: DateTime } deriving (Eq, Ord, Show)

data Description = Description { runDescription :: Maybe String } deriving (Eq, Ord, Show)
data Summary     = Summary     { runSummary     :: Maybe String } deriving (Eq, Ord, Show)
data Location    = Location    { runLocation    :: Maybe String } deriving (Eq, Ord, Show)

data Event = Event {
  dtStamp     :: DtStamp,
  uid         :: Uid,
  dtStart     :: DtStart,
  dtEnd       :: DtEnd,
  description :: Description,
  summary     :: Summary,
  location    :: Location
} deriving (Eq, Ord, Show)

data Version = Version { runVersion :: String } deriving (Eq, Ord, Show)
data ProdId  = ProdId  { runProdId  :: String } deriving (Eq, Ord, Show)

data Calendar = Calendar {
  version :: Version,
  prodId  :: ProdId,
  events  :: [Event]
} deriving (Eq, Ord, Show)

data CalendarDay = EmptyDay
                 | CDay Day deriving (Eq, Ord, Show)
type CalendarDayBlock = (CalendarDay, [Event])

-- Exercise 7
data Token = TBeginCalendar
           | TEndCalendar
           | TBeginEvent
           | TEndEvent
           | TVersion String
           | TProdId String
           | TDtStamp DateTime
           | TUid String
           | TDtStart DateTime
           | TDtEnd DateTime
           | TDescription String
           | TSummary String
           | TLocation String
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
tBeginCalendar :: Parser Char Token
tBeginCalendar = TBeginCalendar <$ token "BEGIN:VCALENDAR" <* newLines

tEndCalendar :: Parser Char Token
tEndCalendar = TEndCalendar <$ token "END:VCALENDAR" <* newLines

tBeginEvent :: Parser Char Token
tBeginEvent = TBeginEvent <$ token "BEGIN:VEVENT" <* newLines

tEndEvent :: Parser Char Token
tEndEvent = TEndEvent <$ token "END:VEVENT" <* newLines

tVersion :: Parser Char Token
tVersion = TVersion . concat <$ token "VERSION:" <*> parseValue

tProdId :: Parser Char Token
tProdId = TProdId . concat <$ token "PRODID:" <*> parseValue

tDtStamp :: Parser Char Token
tDtStamp = TDtStamp <$ token "DTSTAMP:" <*> parseDateTime <* newLines

tUid :: Parser Char Token
tUid = TUid . concat <$ token "UID:" <*> parseValue

tDtStart :: Parser Char Token
tDtStart = TDtStart <$ token "DTSTART:" <*> parseDateTime <* newLines

tDtEnd :: Parser Char Token
tDtEnd = TDtEnd <$ token "DTEND:" <*> parseDateTime <* newLines

tDescription :: Parser Char Token
tDescription = TDescription . concat <$ token "DESCRIPTION:" <*> parseValue

tSummary :: Parser Char Token
tSummary = TSummary . concat <$ token "SUMMARY:" <*> parseValue

tLocation :: Parser Char Token
tLocation = TLocation . concat <$ token "LOCATION:" <*> parseValue

anyToken :: Parser Char Token
anyToken = tBeginCalendar
       <|> tEndCalendar
       <|> tBeginEvent
       <|> tEndEvent
       <|> tVersion
       <|> tProdId
       <|> tDtStamp
       <|> tUid
       <|> tDtStart
       <|> tDtEnd
       <|> tDescription
       <|> tSummary
       <|> tLocation

beginCalendar :: Parser Token ()
beginCalendar = () <$ satisfy (== TBeginCalendar)

endCalendar :: Parser Token ()
endCalendar = () <$ satisfy (== TEndCalendar)

beginEvent :: Parser Token ()
beginEvent = () <$ satisfy (== TBeginEvent)

endEvent :: Parser Token ()
endEvent = () <$ satisfy (== TEndEvent)

-- Calendar
calendarProp :: Parser Token Token
calendarProp = fromCalendarProp <$> satisfy isCalendarProp

isCalendarProp :: Token -> Bool
isCalendarProp (TProdId _)  = True
isCalendarProp (TVersion _) = True
isCalendarProp _            = False

fromCalendarProp :: Token -> Token
fromCalendarProp x@(TProdId _)  = x
fromCalendarProp x@(TVersion _) = x
fromCalendarProp _              = error "fromCalendarProp"

findVersion :: [Token] -> Version
findVersion []              = error "Version not found"
findVersion (TVersion x:xs) = Version x
findVersion (_:xs)          = findVersion xs

findProdId :: [Token] -> ProdId
findProdId []             = error "ProdId not found"
findProdId (TProdId x:xs) = ProdId x
findProdId (_:xs)         = findProdId xs

-- Event
eventProp :: Parser Token Token
eventProp = fromEventProp <$> satisfy isEventProp

isEventProp :: Token -> Bool
isEventProp (TDtStamp _)     = True
isEventProp (TUid _)         = True
isEventProp (TDtStart _)     = True
isEventProp (TDtEnd _)       = True
isEventProp (TDescription _) = True
isEventProp (TSummary _)     = True
isEventProp (TLocation _)    = True
isEventProp _                = False

fromEventProp :: Token -> Token
fromEventProp x@(TDtStamp _)     = x
fromEventProp x@(TUid _)         = x
fromEventProp x@(TDtStart _)     = x
fromEventProp x@(TDtEnd _)       = x
fromEventProp x@(TDescription _) = x
fromEventProp x@(TSummary _)     = x
fromEventProp x@(TLocation _)    = x
fromEventProp _                  = error "fromEventProp"

findDtStamp :: [Token] -> DtStamp
findDtStamp []              = error "DtStamp not found"
findDtStamp (TDtStamp x:xs) = DtStamp x
findDtStamp (_:xs)          = findDtStamp xs

findUid :: [Token] -> Uid
findUid []          = error "Uid not found"
findUid (TUid x:xs) = Uid x
findUid (_:xs)      = findUid xs

findDtStart :: [Token] -> DtStart
findDtStart []              = error "DtStart not found"
findDtStart (TDtStart x:xs) = DtStart x
findDtStart (_:xs)          = findDtStart xs

findDtEnd :: [Token] -> DtEnd
findDtEnd []            = error "DtEnd not found"
findDtEnd (TDtEnd x:xs) = DtEnd x
findDtEnd (_:xs)        = findDtEnd xs

findDescription :: [Token] -> Description
findDescription []                  = Description Nothing
findDescription (TDescription x:xs) = Description (Just x)
findDescription (_:xs)              = findDescription xs

findSummary :: [Token] -> Summary
findSummary []              = Summary Nothing
findSummary (TSummary x:xs) = Summary (Just x)
findSummary (_:xs)          = findSummary xs

findLocation :: [Token] -> Location
findLocation []               = Location Nothing
findLocation (TLocation x:xs) = Location (Just x)
findLocation (_:xs)           = findLocation xs

scanCalendar :: Parser Char [Token]
scanCalendar = greedy anyToken

parseEvent :: Parser Token Event
parseEvent =
        (\eps ->
          Event
            (findDtStamp eps)
            (findUid eps)
            (findDtStart eps)
            (findDtEnd eps)
            (findDescription eps)
            (findSummary eps)
            (findLocation eps))
        <$  beginEvent
        <*> greedy eventProp
        <*  endEvent

parseCalendar :: Parser Token Calendar
parseCalendar =
            (\a b ->
              Calendar
                (findVersion a)
                (findProdId a)
                b)
            <$  beginCalendar
            <*> many calendarProp
            <*> many parseEvent
            <*  endCalendar

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run scanCalendar s >>= \g -> run parseCalendar g

-- Exercise 8
stringBreak :: String -> String
stringBreak s
  | not $ null drop' = take 42 s ++ "\r\n" ++ stringBreak (' ' : drop')
  | otherwise         = s ++ "\r\n"
  where
    drop' = drop 42 s

printLines :: [a] -> (a -> String) -> String
printLines xs f = concatMap (stringBreak . f) xs

printSummary :: Summary -> String
printSummary (Summary (Just s)) = stringBreak("SUMMARY:" ++ s)
printSummary _                  = error "Summary not found"

printUid :: Uid -> String
printUid (Uid s) = stringBreak("UID:" ++ s)

printDtStamp :: DtStamp -> String
printDtStamp (DtStamp dt) = stringBreak("DTSTAMP:" ++ printDateTime dt)

printDtStart :: DtStart -> String
printDtStart (DtStart dt) = stringBreak("DTSTART:" ++ printDateTime dt)

printDtEnd :: DtEnd -> String
printDtEnd (DtEnd dt) = stringBreak("DTEND:" ++ printDateTime dt)

printDescription :: Description -> String
printDescription (Description (Just s)) = stringBreak("DESCRIPTION:" ++ s)
printDescription _                      = ""

printLocation :: Location -> String
printLocation (Location (Just s)) = stringBreak("LOCATION:" ++ s)
printLocation _                   = ""

printEvent :: Event -> String
printEvent e = "BEGIN:VEVENT\r\n"
            ++ printDtStamp     (dtStamp e)
            ++ printUid         (uid e)
            ++ printDtStart     (dtStart e)
            ++ printDtEnd       (dtEnd e)
            ++ printSummary     (summary e)
            ++ printDescription (description e)
            ++ printLocation    (location e)
            ++ "END:VEVENT\r\n"

printVersion :: Version -> String
printVersion (Version s) = stringBreak("VERSION:" ++ s)

printProdId :: ProdId -> String
printProdId (ProdId s) = stringBreak("PRODID:" ++ s)

printCalendar :: Calendar -> String
printCalendar c = "BEGIN:VCALENDAR\r\n"
               ++ printVersion         (version c)
               ++ printProdId          (prodId c)
               ++ concatMap printEvent (events c)
               ++ "END:VCALENDAR\r\n"
