{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Calendar where

import           Data.Char              (isAlpha, isAlphaNum, isAscii,
                                         isControl, isSpace, isSymbol)
import           Data.Either            (partitionEithers)
import           Data.List
import           Data.Maybe             (fromJust, fromMaybe)
import           DateTime
import           Debug.Trace
import           ParseLib.Abstract
import           Prelude                hiding (sequence, ($>), (*>), (<$),
                                         (<*), (<>))
import           Text.PrettyPrint.Boxes


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

type CalendarDay = (Day, [Event])

-- Exercise 7
data Token = Token Calendar
  deriving (Eq, Ord, Show)

-- Test data calendar
calendarString :: String
calendarString = "BEGIN:VCALENDAR\r\nPRODID:/\r\nVERSION:2.0\r\nBEGIN:VEVENT\r\nDTSTART:20121112T121500Z\r\nDTEND:20121112T140000Z\r\nLOCATION:BBL 079\r\nUID:20121106T091853Z@mysite.com\r\nDTSTAMP:20121106T091852Z\r\nSUMMARY:HOORCOL INFOB3TC group: 1\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n HOORCOL group: 1 in: BBL 079\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20121112T141500Z\r\nDTEND:20121112T160000Z\r\nLOCATION:BBL 103 CLZ\r\nUID:20121106T091854Z@mysite.com\r\nDTSTAMP:20121106T091852Z\r\nSUMMARY:PRACT INFOB3TC group: 1\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n PRACT group: 1 in: BBL 103 CLZ\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20121112T141500Z\r\nDTEND:20121112T160000Z\r\nLOCATION:BBL 106 CLZ\r\nUID:20121106T091855Z@mysite.com\r\nDTSTAMP:20121106T091852Z\r\nSUMMARY:PRACT INFOB3TC group: 2\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n PRACT group: 2 in: BBL 106 CLZ\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20121115T080000Z\r\nDTEND:20121115T094500Z\r\nLOCATION:BBL 079\r\nUID:20121106T091856Z@mysite.com\r\nDTSTAMP:20121106T091852Z\r\nSUMMARY:HOORCOL INFOB3TC group: 1\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n HOORCOL group: 1 in: BBL 079\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20121115T100000Z\r\nDTEND:20121115T114500Z\r\nLOCATION:BBL 017\r\nUID:20121106T091857Z@mysite.com\r\nDTSTAMP:20121106T091852Z\r\nSUMMARY:PRACT INFOB3TC group: 1\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n PRACT group: 1 in: BBL 017\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20121115T100000Z\r\nDTEND:20121115T114500Z\r\nLOCATION:MIN 205\r\nUID:20121106T091858Z@mysite.com\r\nDTSTAMP:20121106T091852Z\r\nSUMMARY:PRACT INFOB3TC group: 1\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n PRACT group: 1 in: MIN 205\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20121119T121500Z\r\nDTEND:20121119T140000Z\r\nLOCATION:BBL 079\r\nUID:20121106T091859Z@mysite.com\r\nDTSTAMP:20121106T091852Z\r\nSUMMARY:HOORCOL INFOB3TC group: 1\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n HOORCOL group: 1 in: BBL 079\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20121119T141500Z\r\nDTEND:20121119T160000Z\r\nLOCATION:BBL 103 CLZ\r\nUID:20121106T091900Z@mysite.com\r\nDTSTAMP:20121106T091852Z\r\nSUMMARY:PRACT INFOB3TC group: 1\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n PRACT group: 1 in: BBL 103 CLZ\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20121119T141500Z\r\nDTEND:20121119T160000Z\r\nLOCATION:BBL 106 CLZ\r\nUID:20121106T091901Z@mysite.com\r\nDTSTAMP:20121106T091852Z\r\nSUMMARY:PRACT INFOB3TC group: 2\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n PRACT group: 2 in: BBL 106 CLZ\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20121122T080000Z\r\nDTEND:20121122T094500Z\r\nLOCATION:BBL 079\r\nUID:20121106T091902Z@mysite.com\r\nDTSTAMP:20121106T091852Z\r\nSUMMARY:HOORCOL INFOB3TC group: 1\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n HOORCOL group: 1 in: BBL 079\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20121122T100000Z\r\nDTEND:20121122T114500Z\r\nLOCATION:BBL 017\r\nUID:20121106T091903Z@mysite.com\r\nDTSTAMP:20121106T091852Z\r\nSUMMARY:PRACT INFOB3TC group: 1\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n PRACT group: 1 in: BBL 017\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20121122T100000Z\r\nDTEND:20121122T114500Z\r\nLOCATION:MIN 205\r\nUID:20121106T091904Z@mysite.com\r\nDTSTAMP:20121106T091852Z\r\nSUMMARY:PRACT INFOB3TC group: 1\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n PRACT group: 1 in: MIN 205\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20121126T121500Z\r\nDTEND:20121126T140000Z\r\nLOCATION:BBL 079\r\nUID:20121106T091905Z@mysite.com\r\nDTSTAMP:20121106T091852Z\r\nSUMMARY:HOORCOL INFOB3TC group: 1\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n HOORCOL group: 1 in: BBL 079\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20121126T141500Z\r\nDTEND:20121126T160000Z\r\nLOCATION:BBL 103 CLZ\r\nUID:20121106T091906Z@mysite.com\r\nDTSTAMP:20121106T091852Z\r\nSUMMARY:PRACT INFOB3TC group: 1\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n PRACT group: 1 in: BBL 103 CLZ\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20121126T141500Z\r\nDTEND:20121126T160000Z\r\nLOCATION:BBL 106 CLZ\r\nUID:20121106T091907Z@mysite.com\r\nDTSTAMP:20121106T091852Z\r\nSUMMARY:PRACT INFOB3TC group: 2\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n PRACT group: 2 in: BBL 106 CLZ\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20121129T080000Z\r\nDTEND:20121129T094500Z\r\nLOCATION:BBL 079\r\nUID:20121106T091908Z@mysite.com\r\nDTSTAMP:20121106T091852Z\r\nSUMMARY:HOORCOL INFOB3TC group: 1\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n HOORCOL group: 1 in: BBL 079\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20121129T100000Z\r\nDTEND:20121129T114500Z\r\nLOCATION:BBL 017\r\nUID:20121106T091909Z@mysite.com\r\nDTSTAMP:20121106T091852Z\r\nSUMMARY:PRACT INFOB3TC group: 1\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n PRACT group: 1 in: BBL 017\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20121129T100000Z\r\nDTEND:20121129T114500Z\r\nLOCATION:MIN 205\r\nUID:20121106T091910Z@mysite.com\r\nDTSTAMP:20121106T091852Z\r\nSUMMARY:PRACT INFOB3TC group: 1\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n PRACT group: 1 in: MIN 205\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20121203T121500Z\r\nDTEND:20121203T140000Z\r\nLOCATION:BBL 079\r\nUID:20121106T091911Z@mysite.com\r\nDTSTAMP:20121106T091852Z\r\nSUMMARY:HOORCOL INFOB3TC group: 1\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n HOORCOL group: 1 in: BBL 079\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20121203T141500Z\r\nDTEND:20121203T160000Z\r\nLOCATION:BBL 103 CLZ\r\nUID:20121106T091912Z@mysite.com\r\nDTSTAMP:20121106T091852Z\r\nSUMMARY:PRACT INFOB3TC group: 1\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n PRACT group: 1 in: BBL 103 CLZ\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20121203T141500Z\r\nDTEND:20121203T160000Z\r\nLOCATION:BBL 106 CLZ\r\nUID:20121106T091913Z@mysite.com\r\nDTSTAMP:20121106T091852Z\r\nSUMMARY:PRACT INFOB3TC group: 2\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n PRACT group: 2 in: BBL 106 CLZ\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20121206T080000Z\r\nDTEND:20121206T094500Z\r\nLOCATION:BBL 079\r\nUID:20121106T091914Z@mysite.com\r\nDTSTAMP:20121106T091852Z\r\nSUMMARY:HOORCOL INFOB3TC group: 1\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n HOORCOL group: 1 in: BBL 079\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20121206T100000Z\r\nDTEND:20121206T114500Z\r\nLOCATION:BBL 017\r\nUID:20121106T091915Z@mysite.com\r\nDTSTAMP:20121106T091852Z\r\nSUMMARY:PRACT INFOB3TC group: 1\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n PRACT group: 1 in: BBL 017\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20121206T100000Z\r\nDTEND:20121206T114500Z\r\nLOCATION:MIN 205\r\nUID:20121106T091916Z@mysite.com\r\nDTSTAMP:20121106T091852Z\r\nSUMMARY:PRACT INFOB3TC group: 1\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n PRACT group: 1 in: MIN 205\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20121210T121500Z\r\nDTEND:20121210T140000Z\r\nLOCATION:BBL 079\r\nUID:20121106T091917Z@mysite.com\r\nDTSTAMP:20121106T091852Z\r\nSUMMARY:HOORCOL INFOB3TC group: 1\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n HOORCOL group: 1 in: BBL 079\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20121210T141500Z\r\nDTEND:20121210T160000Z\r\nLOCATION:BBL 103 CLZ\r\nUID:20121106T091918Z@mysite.com\r\nDTSTAMP:20121106T091852Z\r\nSUMMARY:PRACT INFOB3TC group: 1\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n PRACT group: 1 in: BBL 103 CLZ\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20121210T141500Z\r\nDTEND:20121210T160000Z\r\nLOCATION:BBL 106 CLZ\r\nUID:20121106T091919Z@mysite.com\r\nDTSTAMP:20121106T091852Z\r\nSUMMARY:PRACT INFOB3TC group: 2\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n PRACT group: 2 in: BBL 106 CLZ\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20121213T073000Z\r\nDTEND:20121213T093000Z\r\nLOCATION:EDUC BETA\r\nUID:20121106T091921Z@mysite.com\r\nDTSTAMP:20121106T091853Z\r\nSUMMARY:TOETS INFOB3TC group: 0\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n TOETS group: 0 in: EDUC BETA\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20121213T080000Z\r\nDTEND:20121213T094500Z\r\nLOCATION:BBL 079\r\nUID:20121106T091922Z@mysite.com\r\nDTSTAMP:20121106T091853Z\r\nSUMMARY:HOORCOL INFOB3TC group: 1\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n HOORCOL group: 1 in: BBL 079\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20121213T100000Z\r\nDTEND:20121213T114500Z\r\nLOCATION:BBL 017\r\nUID:20121106T091923Z@mysite.com\r\nDTSTAMP:20121106T091853Z\r\nSUMMARY:PRACT INFOB3TC group: 1\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n PRACT group: 1 in: BBL 017\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20121213T100000Z\r\nDTEND:20121213T114500Z\r\nLOCATION:MIN 205\r\nUID:20121106T091924Z@mysite.com\r\nDTSTAMP:20121106T091853Z\r\nSUMMARY:PRACT INFOB3TC group: 1\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n PRACT group: 1 in: MIN 205\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20121217T121500Z\r\nDTEND:20121217T140000Z\r\nLOCATION:BBL 079\r\nUID:20121106T091925Z@mysite.com\r\nDTSTAMP:20121106T091853Z\r\nSUMMARY:HOORCOL INFOB3TC group: 1\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n HOORCOL group: 1 in: BBL 079\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20121217T141500Z\r\nDTEND:20121217T160000Z\r\nLOCATION:BBL 103 CLZ\r\nUID:20121106T091926Z@mysite.com\r\nDTSTAMP:20121106T091853Z\r\nSUMMARY:PRACT INFOB3TC group: 1\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n PRACT group: 1 in: BBL 103 CLZ\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20121217T141500Z\r\nDTEND:20121217T160000Z\r\nLOCATION:BBL 106 CLZ\r\nUID:20121106T091927Z@mysite.com\r\nDTSTAMP:20121106T091853Z\r\nSUMMARY:PRACT INFOB3TC group: 2\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n PRACT group: 2 in: BBL 106 CLZ\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20121220T080000Z\r\nDTEND:20121220T094500Z\r\nLOCATION:BBL 079\r\nUID:20121106T091928Z@mysite.com\r\nDTSTAMP:20121106T091853Z\r\nSUMMARY:HOORCOL INFOB3TC group: 1\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n HOORCOL group: 1 in: BBL 079\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20121220T100000Z\r\nDTEND:20121220T114500Z\r\nLOCATION:BBL 017\r\nUID:20121106T091929Z@mysite.com\r\nDTSTAMP:20121106T091853Z\r\nSUMMARY:PRACT INFOB3TC group: 1\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n PRACT group: 1 in: BBL 017\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20121220T100000Z\r\nDTEND:20121220T114500Z\r\nLOCATION:MIN 205\r\nUID:20121106T091930Z@mysite.com\r\nDTSTAMP:20121106T091853Z\r\nSUMMARY:PRACT INFOB3TC group: 1\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n PRACT group: 1 in: MIN 205\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20130107T121500Z\r\nDTEND:20130107T140000Z\r\nLOCATION:BBL 079\r\nUID:20121106T091931Z@mysite.com\r\nDTSTAMP:20121106T091853Z\r\nSUMMARY:HOORCOL INFOB3TC group: 1\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n HOORCOL group: 1 in: BBL 079\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20130107T141500Z\r\nDTEND:20130107T160000Z\r\nLOCATION:BBL 103 CLZ\r\nUID:20121106T091932Z@mysite.com\r\nDTSTAMP:20121106T091853Z\r\nSUMMARY:PRACT INFOB3TC group: 1\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n PRACT group: 1 in: BBL 103 CLZ\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20130107T141500Z\r\nDTEND:20130107T160000Z\r\nLOCATION:BBL 106 CLZ\r\nUID:20121106T091933Z@mysite.com\r\nDTSTAMP:20121106T091853Z\r\nSUMMARY:PRACT INFOB3TC group: 2\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n PRACT group: 2 in: BBL 106 CLZ\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20130110T080000Z\r\nDTEND:20130110T094500Z\r\nLOCATION:BBL 079\r\nUID:20121106T091934Z@mysite.com\r\nDTSTAMP:20121106T091853Z\r\nSUMMARY:HOORCOL INFOB3TC group: 1\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n HOORCOL group: 1 in: BBL 079\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20130110T100000Z\r\nDTEND:20130110T114500Z\r\nLOCATION:BBL 017\r\nUID:20121106T091935Z@mysite.com\r\nDTSTAMP:20121106T091853Z\r\nSUMMARY:PRACT INFOB3TC group: 1\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n PRACT group: 1 in: BBL 017\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20130110T100000Z\r\nDTEND:20130110T114500Z\r\nLOCATION:MIN 205\r\nUID:20121106T091936Z@mysite.com\r\nDTSTAMP:20121106T091853Z\r\nSUMMARY:PRACT INFOB3TC group: 1\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n PRACT group: 1 in: MIN 205\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20130114T121500Z\r\nDTEND:20130114T140000Z\r\nLOCATION:BBL 079\r\nUID:20121106T091937Z@mysite.com\r\nDTSTAMP:20121106T091853Z\r\nSUMMARY:HOORCOL INFOB3TC group: 1\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n HOORCOL group: 1 in: BBL 079\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20130114T141500Z\r\nDTEND:20130114T160000Z\r\nLOCATION:BBL 103 CLZ\r\nUID:20121106T091938Z@mysite.com\r\nDTSTAMP:20121106T091853Z\r\nSUMMARY:PRACT INFOB3TC group: 1\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n PRACT group: 1 in: BBL 103 CLZ\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20130114T141500Z\r\nDTEND:20130114T160000Z\r\nLOCATION:BBL 106 CLZ\r\nUID:20121106T091939Z@mysite.com\r\nDTSTAMP:20121106T091853Z\r\nSUMMARY:PRACT INFOB3TC group: 2\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n PRACT group: 2 in: BBL 106 CLZ\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20130117T080000Z\r\nDTEND:20130117T094500Z\r\nLOCATION:BBL 079\r\nUID:20121106T091940Z@mysite.com\r\nDTSTAMP:20121106T091853Z\r\nSUMMARY:HOORCOL INFOB3TC group: 1\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n HOORCOL group: 1 in: BBL 079\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20130117T100000Z\r\nDTEND:20130117T114500Z\r\nLOCATION:BBL 017\r\nUID:20121106T091941Z@mysite.com\r\nDTSTAMP:20121106T091853Z\r\nSUMMARY:PRACT INFOB3TC group: 1\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n PRACT group: 1 in: BBL 017\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20130117T100000Z\r\nDTEND:20130117T114500Z\r\nLOCATION:MIN 205\r\nUID:20121106T091942Z@mysite.com\r\nDTSTAMP:20121106T091853Z\r\nSUMMARY:PRACT INFOB3TC group: 1\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n PRACT group: 1 in: MIN 205\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20130121T121500Z\r\nDTEND:20130121T140000Z\r\nLOCATION:BBL 079\r\nUID:20121106T091943Z@mysite.com\r\nDTSTAMP:20121106T091853Z\r\nSUMMARY:HOORCOL INFOB3TC group: 1\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n HOORCOL group: 1 in: BBL 079\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20130121T141500Z\r\nDTEND:20130121T160000Z\r\nLOCATION:BBL 103 CLZ\r\nUID:20121106T091944Z@mysite.com\r\nDTSTAMP:20121106T091853Z\r\nSUMMARY:PRACT INFOB3TC group: 1\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n PRACT group: 1 in: BBL 103 CLZ\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20130121T141500Z\r\nDTEND:20130121T160000Z\r\nLOCATION:BBL 106 CLZ\r\nUID:20121106T091945Z@mysite.com\r\nDTSTAMP:20121106T091853Z\r\nSUMMARY:PRACT INFOB3TC group: 2\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n PRACT group: 2 in: BBL 106 CLZ\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20130124T080000Z\r\nDTEND:20130124T094500Z\r\nLOCATION:BBL 079\r\nUID:20121106T091946Z@mysite.com\r\nDTSTAMP:20121106T091853Z\r\nSUMMARY:HOORCOL INFOB3TC group: 1\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n HOORCOL group: 1 in: BBL 079\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20130124T100000Z\r\nDTEND:20130124T114500Z\r\nLOCATION:BBL 017\r\nUID:20121106T091947Z@mysite.com\r\nDTSTAMP:20121106T091853Z\r\nSUMMARY:PRACT INFOB3TC group: 1\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n PRACT group: 1 in: BBL 017\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20130124T100000Z\r\nDTEND:20130124T114500Z\r\nLOCATION:MIN 205\r\nUID:20121106T091948Z@mysite.com\r\nDTSTAMP:20121106T091853Z\r\nSUMMARY:PRACT INFOB3TC group: 1\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n PRACT group: 1 in: MIN 205\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20130131T160000Z\r\nDTEND:20130131T190000Z\r\nLOCATION:EDUC GAMMA\r\nUID:20121106T091949Z@mysite.com\r\nDTSTAMP:20121106T091853Z\r\nSUMMARY:TOETS INFOB3TC group: 0\r\nDESCRIPTION:INFOB3TC: Talen en compilers\r\n TOETS group: 0 in: EDUC GAMMA\r\nEND:VEVENT\r\nEND:VCALENDAR\r\n"
-- calendarString = "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:www.testMeiCalendar.net\r\nBEGIN:VEVENT\r\nDTSTART:20101231T230000\r\nDTEND:20110101T010000\r\nSUMMARY:New Years Eve Reminder\r\nLOCATION:Downtown\r\nDESCRIPTION:Let's get together for New Years Eve\r\nUID:ABCD1234\r\nDTSTAMP:20101125T112600\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20111231T230000\r\nDTEND:20120101T010000\r\nSUMMARY:New Years Eve\r\nDESCRIPTION:Another party!!!\r\nUID:ABCD1235\r\nDTSTAMP:20111125T112600\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nDTSTART:20151231T230000\r\nDTEND:20160101T010000\r\nSUMMARY:NYE\r\nLOCATION:My place\r\nUID:ABCD1236\r\nDTSTAMP:20151125T112601\r\nEND:VEVENT\r\nEND:VCALENDAR\r\n"
-- calendarString = "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:-//hacksw/handcal//NONSGML v1.0//EN\r\nBEGIN:VEVENT\r\nUID:12345@example.com\r\nDTSTAMP:20111205T170000Z\r\nDTSTART:20111205T170000Z\r\nDTEND:20111205T210000Z\r\nSUMMARY:This is a very long description th\r\n at spans over multiple lines.\r\nEND:VEVENT\r\nEND:VCALENDAR\r\n"

calendar :: Calendar
calendar = fromJust $ recognizeCalendar calendarString

calendarStaysSame :: Bool
calendarStaysSame = printCalendar calendar == calendarString

printCalendarTest :: IO ()
printCalendarTest = putStrLn $ printCalendar calendar

eventDt :: DateTime
eventDt = (DateTime {date = Date {year = Year {runYear = 1997}, month = Month {runMonth = 7}, day = Day {runDay = 15}}, time = Time {hour = Hour {runHour = 3}, minute = Minute {runMinute = 0}, second = Second {runSecond = 0}}, utc = True})

test :: [Event]
test = calendarEventsInRange calendar eventDt

firstEvent :: Event
firstEvent = head $ events calendar

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

character :: Parser Char [String]
character = (:) <$> identifier' <* newLines <*> character'
  where
    character' = greedy (symbol ' ' *> identifier' <* newLines)

-- Calendar
parseVersion :: Parser Char CalendarProp
parseVersion = Version . concat <$ token "VERSION:" <*> character

parseProdId :: Parser Char CalendarProp
parseProdId = ProdId . concat <$ token "PRODID:" <*> character

parseCalendarProp :: Parser Char CalendarProp
parseCalendarProp = parseProdId <|> parseVersion

-- Event
parseDtStamp :: Parser Char EventProp
parseDtStamp = DtStamp <$ token "DTSTAMP:" <*> parseDateTime <* newLines

parseUid :: Parser Char EventProp
parseUid = Uid . concat <$ token "UID:" <*> character

parseDtStart :: Parser Char EventProp
parseDtStart = DtStart <$ token "DTSTART:" <*> parseDateTime <* newLines

parseDtEnd :: Parser Char EventProp
parseDtEnd = DtEnd <$ token "DTEND:" <*> parseDateTime <* newLines

parseDescription :: Parser Char EventProp
parseDescription = Description . concat <$ token "DESCRIPTION:" <*> character

parseSummary :: Parser Char EventProp
parseSummary = Summary . concat <$ token "SUMMARY:" <*> character

parseLocation :: Parser Char EventProp
parseLocation = Location . concat <$ token "LOCATION:" <*> character

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

-- Exercise 9
calendarEventCount :: Calendar -> Int
calendarEventCount (Calendar { events = es }) = length es

getDtStart :: Event -> DateTime
getDtStart (Event es) = case find f es of
  Nothing           -> error "Unexpected: dtStart not found"
  Just (DtStart dt) -> dt
  where
    f (DtStart _) = True
    f _           = False

getDtEnd :: Event -> DateTime
getDtEnd (Event es) = case find f es of
  Nothing         -> error "Unexpected: dtEnd not found"
  Just (DtEnd dt) -> dt
  where
    f (DtEnd _) = True
    f _         = False

calendarEventsInRange :: Calendar -> DateTime -> [Event]
calendarEventsInRange (Calendar { events = es }) dt = filter f es
  where
    f e = dt >= getDtStart e && dt < getDtEnd e

eventOverlaps :: Event -> Event -> Bool
eventOverlaps e1 e2 =
  (dts1 <= dts2 && dts2 <= dte1) ||
  (dts2 <= dts1 && dts1 <= dte2)
  where
    dts1 = getDtStart e1
    dts2 = getDtStart e2
    dte1 = getDtEnd e1
    dte2 = getDtEnd e2

hasOverlappingEvents :: Calendar -> Bool
hasOverlappingEvents (Calendar { events = es }) = hasOverlappingEvents' es
  where
    hasOverlappingEvents' []     = False
    hasOverlappingEvents' (x:xs) =
      case find (eventOverlaps x) xs of
        Nothing -> hasOverlappingEvents' xs
        Just _  -> True

eventDuration :: Event -> DateTime
eventDuration e = DateTime
  (Date
    (Year  (runYear  (year de)  - runYear  (year ds)))
    (Month (runMonth (month de) - runMonth (month ds)))
    (Day   (runDay   (day de)   - runDay   (day ds))))
  (Time
    (Hour   (runHour   (hour te)   - runHour   (hour ts)))
    (Minute (runMinute (minute te) - runMinute (minute ts)))
    (Second (runSecond (second te) - runSecond (second ts))))
  utc
  where
    (DateTime ds ts utc) = getDtStart e
    (DateTime de te _  ) = getDtEnd e

dateTimeToMinutes :: DateTime -> Int
dateTimeToMinutes (DateTime
  (Date
    (Year y)
    (Month m)
    (Day d))
  (Time
    (Hour h)
    (Minute mi)
    _) _) =
  (y * 365 * 24 * 60) + (m * 30 * 24 * 60) + (d * 24 * 60) + (h * 60) + mi

getTotalEventDuration :: Calendar -> String -> Int
getTotalEventDuration (Calendar { events = es }) summary =
  sum $ map (dateTimeToMinutes . eventDuration) (filter f es)
  where
    f (Event ps) = case find g ps of
      Nothing -> False
      Just _  -> True
    g (Summary s) = s == summary
    g _           = False

table :: [[CalendarDay]] -> Box
table sss = sep // punctuateV left sep cs // sep where
    sep = text (take width (cycle ("+" ++ replicate 14 '-')))
    width = 14 * length (head sss) + 8
    cs = map column sss

dayBlock :: CalendarDay -> Box
dayBlock (Day d, es) = vcat left (renderDay : renderEvents)
  where
    renderDay = alignHoriz left 14 $ text $ show d
    renderEvents = map (alignHoriz left 14 . text . ppEvent) es

column :: [CalendarDay] -> Box
column ss = sep <> punctuateH left sep (map dayBlock ss) <> sep
  where
    sep = vtext $ replicate height '|'
    height = 1 + maxPosOn (\(_, es) -> length es) ss

vtext :: String -> Box
vtext = vcat left . map char

maxPosOn :: (a -> Int) -> [a] -> Int
maxPosOn f = maximum . (0:) . map f

reshape :: Int -> [a] -> [[a]]
reshape n = unfoldr phi where
    phi [] = Nothing
    phi xs = Just (splitAt n xs)

getCalendarMonthEvents :: Calendar -> Year -> Month -> [Event]
getCalendarMonthEvents (Calendar { events = es }) y m = filter f es
  where
    f e = y == y' && m == m'
      where
        (DateTime (Date y' m' _) _ _) = getDtStart e

filterEventsDay :: [Event] -> Day -> [Event]
filterEventsDay es d = filter f es
  where
    f e = d == d'
      where
        (DateTime (Date _ _ d') _ _) = getDtStart e

ppEvent :: Event -> String
ppEvent (Event es) = ts ++ "-" ++ te
  where
    ts = humanReadableTime $ getDtStart (Event es)
    te = humanReadableTime $ getDtEnd (Event es)

ppDay :: Day -> [Event] -> String
ppDay (Day d) es = show d ++ concatMap ppEvent es

ppMonth :: Year -> Month -> String
ppMonth y m = render $ table (reshape 7 [(Day d, filterEventsDay events (Day d)) | d <- [1..days]])
  where
    filter' = filterEventsDay events (Day 12)
    events = getCalendarMonthEvents calendar y m
    days = getDays y m

test' = putStrLn $ ppMonth (Year 2012) (Month 11)
