{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module EVE.API.XML.Map(
         MapAPI
       , initMapAPI
       , getMapJumpCounts
       , getMapKillCounts
       )
 where

import EVE.API.XML.Helpers(cachedFetcher)
import EVE.Static.Database.TypeIds(TypeId)
import Text.Read(readMaybe)
import Text.XML.Light(Element, findElements, findAttr)
import           EVE.Console(Console)

data MapAPI = MapAPI {
       getMapJumpCounts :: IO (Either String [(TypeId, Word)])
     , getMapKillCounts :: IO (Either String [(TypeId, Word, Word, Word)])
     }

initMapAPI :: Console -> IO MapAPI
initMapAPI con =
  do getMapJumpCounts <- cachedFetcher con "map/Jumps" [] readJumpInfo
     getMapKillCounts <- cachedFetcher con "map/Kills" [] readKillInfo
     return MapAPI{..}

readJumpInfo :: Element -> Maybe [(TypeId, Word)]
readJumpInfo el = sequence $ map parseRow (findElements "row" el)
 where
  parseRow x =
    do solarSystemId <- readMaybe =<< findAttr "solarSystemID" x
       shipJumps     <- readMaybe =<< findAttr "shipJumps"     x
       return (fromIntegral (solarSystemId :: Word), shipJumps)

readKillInfo :: Element -> Maybe [(TypeId, Word, Word, Word)]
readKillInfo el = sequence $ map parseRow (findElements "row" el)
 where
  parseRow x =
    do solarSystemId <- readMaybe =<< findAttr "solarSystemID" x
       ships         <- readMaybe =<< findAttr "shipKills"     x
       facs          <- readMaybe =<< findAttr "factionKills"  x
       pods          <- readMaybe =<< findAttr "podKills"      x
       return (fromIntegral (solarSystemId :: Word), ships, facs, pods)

