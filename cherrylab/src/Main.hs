{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<$>))
import Control.Monad (join)
import Network.HTTP.Types.Status (statusIsSuccessful)
import Control.Lens
import Data.Char (toLower)
import Network.Wreq
import qualified Data.ByteString.Lazy.Char8 as B8 (lines, map, all, unpack)
import qualified Data.ByteString.Lazy as B (readFile, ByteString)
import Debug.Trace (trace)

getDictionary :: IO [String]
getDictionary = join
            <$> map (\x -> [x, x ++ ".pdf"])
            <$> map B8.unpack
            <$> filter noApostrophies
            <$> map (B8.map toLower)
            <$> B8.lines
            <$> B.readFile "/usr/share/dict/words"
    where noApostrophies :: B.ByteString -> Bool
          noApostrophies = B8.all (/= '\'')

responseGotSomething :: Response a -> Bool
responseGotSomething res = statusIsSuccessful $ res ^. responseStatus

findTheDamnAddress :: [String] -> IO String
findTheDamnAddress (x:xs) = trace x $ do
    let opts = defaults & checkResponse .~ Just (\_ _ -> return ())
    response <- getWith opts x
    if responseGotSomething response then
        return x
    else
        findTheDamnAddress xs

main = do
    let urlBase = "http://www.cs.rpi.edu/academics/courses/fall17/csci1200/labs/14_smart_memory/"
    words <- getDictionary
    let urls = [urlBase ++ word | word <- words]
    address <- findTheDamnAddress urls
    print address
