module PrettyDates (PrettyDate, prettifyDate) where
import Data.Dates

newtype PrettyDate = PrettyDate String deriving (Show)

prettifyDate :: DateTime -> PrettyDate
prettifyDate date = PrettyDate pretty
  where pretty = (show $ dateWeekDay date)
              ++ " the " ++ (show $ day date) ++ ordinalSuffix (day date)
              ++ " of " ++ prettyMonth date
              ++ ", " ++ (show $ year date)
        ordinalSuffix :: Int -> String
        ordinalSuffix 11 = "th"
        ordinalSuffix 12 = "th"
        ordinalSuffix 13 = "th"
        ordinalSuffix n
          | n `mod` 10 == 1 = "st"
          | n `mod` 10 == 2 = "nd"
          | n `mod` 10 == 3 = "rd"
          | otherwise = "th"
        prettyMonth :: DateTime -> String
        prettyMonth date
          | month date == 1  = "January"
          | month date == 2  = "February"
          | month date == 3  = "March"
          | month date == 4  = "April"
          | month date == 5  = "May"
          | month date == 6  = "June"
          | month date == 7  = "July"
          | month date == 8  = "August"
          | month date == 9  = "September"
          | month date == 10 = "October"
          | month date == 11 = "November"
          | month date == 12 = "December"
