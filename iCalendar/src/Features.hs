module Features where

import           Calendar
import           Data.List
import qualified Data.Time              as DT
import           Data.Time.Format       as DT
import           DateTime
import           GHC.Num                (integerFromInt)
import           Prelude                hiding ((<>))
import           Text.PrettyPrint.Boxes

-- Exercise 9
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

eventOverlaps :: Event -> Event -> Bool
eventOverlaps e1 e2 =
  (dts1 <= dts2 && dts2 <= dte1) ||
  (dts2 <= dts1 && dts1 <= dte2)
  where
    dts1 = getDtStart e1
    dts2 = getDtStart e2
    dte1 = getDtEnd e1
    dte2 = getDtEnd e2

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

countEvents :: Calendar -> Int
countEvents (Calendar { events = es }) = length es

findEvents :: DateTime -> Calendar -> [Event]
findEvents dt (Calendar { events = es }) = filter f es
  where
    f e = dt >= getDtStart e && dt < getDtEnd e

checkOverlapping :: Calendar -> Bool
checkOverlapping (Calendar { events = es }) = hasOverlappingEvents' es
  where
    hasOverlappingEvents' []     = False
    hasOverlappingEvents' (x:xs) =
      case find (eventOverlaps x) xs of
        Nothing -> hasOverlappingEvents' xs
        Just _  -> True

timeSpent :: String -> Calendar -> Int
timeSpent summary (Calendar { events = es }) =
  sum $ map (dateTimeToMinutes . eventDuration) (filter f es)
  where
    f (Event ps) = case find g ps of
      Nothing -> False
      Just _  -> True
    g (Summary s) = s == summary
    g _           = False

firstDayOfMonth :: Year -> Month -> Day
firstDayOfMonth (Year y) (Month m) = Day weekday
  where
    date = DT.fromGregorian (integerFromInt y) m 1
    weekday = read $ DT.formatTime DT.defaultTimeLocale "%u" date :: Int

humanReadableTime :: DateTime -> String
humanReadableTime (DateTime _ (Time (Hour h) (Minute m) _) _) =
  showTwoDigit h ++ ":" ++ showTwoDigit m

-- Exercise 10
paddingSize,
  contentSize,
  totalSize :: Int
paddingSize = 1
contentSize = 11
totalSize   = contentSize + (2 * paddingSize)

reshape :: Int -> [a] -> [[a]]
reshape n = unfoldr split
  where
    split [] = Nothing
    split xs = Just (splitAt n xs)

vtext :: String -> Box
vtext = vcat left . map char

maximum' :: (a -> Int) -> [a] -> Int
maximum' _ [] = 0
maximum' f xs = (maximum . map f) xs

ppEvent :: Event -> String
ppEvent (Event es) = ts ++ "-" ++ te
  where
    ts = humanReadableTime $ getDtStart (Event es)
    te = humanReadableTime $ getDtEnd (Event es)

emptyLine :: Box
emptyLine = emptyBox 1 1

renderDay :: CalendarDayBlock -> Box
renderDay (EmptyDay, _) = emptyBox 1 totalSize
renderDay (CDay (Day d), es) = padding <> vcat left (day : emptyLine : events) <> padding
  where
    padding = text $ replicate paddingSize ' '
    day = alignHoriz left contentSize $ text $ show d
    events = map (alignHoriz left contentSize . text . ppEvent) es

renderWeek :: [CalendarDayBlock] -> Box
renderWeek ss = sep <> punctuateH left sep daysBoxes <> sep
  where
    daysBoxes = map renderDay ss
    height = maximum' rows daysBoxes
    sep = vtext $ replicate height '|'

renderHeader :: Year -> Month -> Box
renderHeader (Year y) (Month m) = sep <> vcat left boxes <> sep
  where
    date = DT.fromGregorian (integerFromInt y) m 1
    weekday = DT.formatTime DT.defaultTimeLocale "%B %Y" date
    boxes = [
        emptyLine,
        alignHoriz center1 (totalSize * 7 + 6) (text weekday),
        emptyLine
      ]
    sep = vtext $ replicate (length boxes) '|'

renderWeekdays :: Box
renderWeekdays = sep <> punctuateH left sep (map (alignHoriz center1 totalSize . text . show) [DT.Monday .. DT.Sunday]) <> sep
  where
    sep = char '|'

renderTable :: Year -> Month -> [[CalendarDayBlock]] -> Box
renderTable y m cds = sep // punctuateV left sep (renderHeader y m : renderWeekdays : weeks) // sep
  where
    width = totalSize * length (head cds) + 8
    sep = text (take width (cycle ("+" ++ replicate totalSize '-')))
    weeks = map renderWeek cds

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

ppMonth :: Year -> Month -> Calendar -> String
ppMonth y m c = render (renderTable y m (reshape 7 $ fillStartDays ++ [createCDay d | d <- [1..(35-skipDays)]]))
  where
    emptyDay = (EmptyDay, [])
    events = getCalendarMonthEvents c y m
    days = getDays y m
    skipDays = let Day d = firstDayOfMonth y m in d-1
    fillStartDays = replicate skipDays emptyDay
    createCDay d
      | d <= days = (CDay $ Day d, filterEventsDay events (Day d))
      | otherwise = emptyDay

-- Test functions
test = putStrLn $ ppMonth (Year 2012) (Month 11) calendar
test2 = putStrLn $ ppMonth (Year 2023) (Month 11) calendar
