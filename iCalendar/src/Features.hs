module Features where

import           Calendar
import           Data.List
import           Data.Maybe
import qualified Data.Time              as DT
import           Data.Time.Format       as DT
import           DateTime
import           GHC.Num                (integerFromInt)
import           Prelude                hiding ((<>))
import           Text.PrettyPrint.Boxes

-- Exercise 9
eventOverlaps :: Event -> Event -> Bool
eventOverlaps e1 e2 =
  (dts1 <= dts2 && dts2 <= dte1) ||
  (dts2 <= dts1 && dts1 <= dte2)
  where
    dts1 = runDtStart $ dtStart e1
    dts2 = runDtStart $ dtStart e2
    dte1 = runDtEnd   $ dtEnd e1
    dte2 = runDtEnd   $ dtEnd e2

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
    (DtStart (DateTime ds ts utc)) = dtStart e
    (DtEnd   (DateTime de te _  )) = dtEnd e

yearToMinutes :: Year -> Year -> Int
yearToMinutes _ (Year 0) = 0
yearToMinutes (Year sy) (Year y) = getDaysInYear (Year sy) * 24 * 60 + yearToMinutes (Year nextYear) (Year $ y - 1)
  where
    nextYear = sy + 1

monthToMinutes :: Year -> Month -> Month -> Int
monthToMinutes _ _ (Month 0) = 0
monthToMinutes (Year sy) (Month sm) (Month m) =
  getDays (Year sy) (Month sm) * 24 * 60 + monthToMinutes (Year nextYear) (Month nextMonth) (Month m)
  where
    nextMonth
      | sm == 12  = 1
      | otherwise = sm + 1
    nextYear
      | sm == 12  = sy + 1
      | otherwise = sy

dateTimeToMinutes :: Year -> Month -> DateTime -> Int
dateTimeToMinutes startYear startMonth (DateTime
  (Date
    (Year y)
    (Month m)
    (Day d))
  (Time
    (Hour h)
    (Minute mi)
    _) _) =
  yearToMinutes startYear (Year y)  + monthToMinutes startYear startMonth (Month m) + (d * 24 * 60) + (h * 60) + mi

countEvents :: Calendar -> Int
countEvents (Calendar { events = es }) = length es

findEvents :: DateTime -> Calendar -> [Event]
findEvents dt (Calendar { events = es }) = filter f es
  where
    f e = dt >= runDtStart (dtStart e) && dt < runDtEnd (dtEnd e)

checkOverlapping :: Calendar -> Bool
checkOverlapping (Calendar { events = es }) = hasOverlappingEvents' es
  where
    hasOverlappingEvents' []     = False
    hasOverlappingEvents' (x:xs) =
      case find (eventOverlaps x) xs of
        Nothing -> hasOverlappingEvents' xs
        Just _  -> True

timeSpent :: String -> Calendar -> Int
timeSpent summary' (Calendar { events = es }) =
  sum $ map (\e -> dateTimeToMinutes
                      (year $ dateStart e)
                      (month $ dateStart e)
                      $ eventDuration e) (filter f es)
  where
    dateStart = date . runDtStart . dtStart
    f e = case runSummary (summary e) of
      Nothing        -> False
      Just summary'' -> summary'' == summary'

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

uniqueDayId :: Event -> String
uniqueDayId e = " #" ++ show ((day' + 23) * hour' `mod` 1000)
  where
    dts = runDtStart $ dtStart e
    day' = runDay $ day $ date dts
    hour' = runHour $ hour $ time dts

ppEvent :: Day -> Event -> String
ppEvent d e = ts ++ "-" ++ te ++ uniqueDayId e
  where
    ts
      | d == day (date $ runDtStart $ dtStart e) = humanReadableTime $ runDtStart $ dtStart e
      | otherwise = ""
    te
      | d == day (date $ runDtEnd $ dtEnd e) = humanReadableTime $ runDtEnd $ dtEnd e
      | otherwise = ""


emptyLine :: Box
emptyLine = emptyBox 1 1

renderDay :: CalendarDayBlock -> Box
renderDay (EmptyDay, _) = emptyBox 1 totalSize
renderDay (CDay (Day d), es) = padding <> vcat left (day : emptyLine : event) <> padding
  where
    padding = text $ replicate paddingSize ' '
    day = alignHoriz left contentSize $ text $ show d
    event = map (alignHoriz left contentSize . text . ppEvent (Day d)) es

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
        (DateTime (Date y' m' _) _ _) = runDtStart $ dtStart e

filterEventsDay :: [Event] -> Day -> [Event]
filterEventsDay es d = filter f es
  where
    f e = d == d' || d == d''
      where
        (DateTime (Date _ _ d') _ _)  = runDtStart $ dtStart e
        (DateTime (Date _ _ d'') _ _) = runDtEnd   $ dtEnd e

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

-- Test functions for myself
bastillePath = "examples/bastille.ics"
multiLinePath = "examples/multiline.ics"
newYearPath = "examples/newyear.ics"
roosterPath = "examples/rooster_infotc.ics"

testPrint year month filepath = do
  calendar <- readCalendar filepath
  putStrLn $ ppMonth (Year year) (Month month) (fromJust calendar)
