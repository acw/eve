{-# LANGUAGE OverloadedStrings #-}
module EVE.API.XML.MapJumps(
         parse
       )
 where

import EVE.API.XML.Helpers(cachedFetcher)
import Text.Read(readMaybe)
import Text.XML.Light(Element, findElements, findAttr)

readJumpInfo :: Element -> Maybe [(Word, Word)]
readJumpInfo el = sequence $ map parseRow (findElements "row" el)
 where
  parseRow x =
    do solarSystemId <- readMaybe =<< findAttr "solarSystemID" x
       shipJumps     <- readMaybe =<< findAttr "shipJumps"     x
       return (solarSystemId, shipJumps)

parse :: IO ()
parse =
  do fetch <- cachedFetcher "map/Jumps" [] readJumpInfo
     x <- fetch
     putStrLn ("Fetched: " ++ show x)
     y <- fetch
     putStrLn ("Fetched: " ++ show y)
