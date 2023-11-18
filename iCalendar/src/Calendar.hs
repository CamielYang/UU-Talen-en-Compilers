module Calendar where

import           DateTime
import           ParseLib.Abstract
import           Prelude           hiding (sequence, ($>), (*>), (<$), (<*))


-- Exercise 6
data Calendar = Calendar {
    version :: String,
    pid     :: String,
    events  :: [Event]
}
    deriving (Eq, Ord, Show)

data Event = Event {
    dtstamp     :: DateTime,
    uid         :: String,
    dtstart     :: DateTime,
    dtend       :: DateTime,
    description :: String,
    summary     :: String,
    location    :: String
} deriving (Eq, Ord, Show)

-- Exercise 7
data Token = Token
    deriving (Eq, Ord, Show)

scanCalendar :: Parser Char [Token]
scanCalendar = undefined

parseCalendar :: Parser Token Calendar
parseCalendar = undefined

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run scanCalendar s >>= run parseCalendar

-- Exercise 8
printCalendar :: Calendar -> String
printCalendar = undefined
