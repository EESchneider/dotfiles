{-# LANGUAGE TemplateHaskell #-}

import Data.Functor ((<$>))
import Control.Lens
import Data.Dates (getCurrentDateTime)
import PrettyDates

data Entry = Entry { _date :: PrettyDate, _text :: String } deriving (Show)
newtype Journal = Journal { _entries :: [Entry] } deriving (Show)
makeLenses ''Entry
makeLenses ''Journal

writeEntry :: Journal -> String -> IO Journal
writeEntry journal text = do
  date <- prettifyDate <$> getCurrentDateTime
  let entry = Entry date text
  return $ Journal (entry:journal ^. entries)

main = do
  let journal0 = Journal []
  journal1 <- writeEntry journal0 "foo"
  print $ journal1
