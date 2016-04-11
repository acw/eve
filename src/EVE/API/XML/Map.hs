{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module EVE.API.XML.Map(
         MapAPI
       , initMapAPI
       , getMapJumpCounts
       , getMapKillCounts
       , KillCounts
       , emptyKillCounts
       , kcShipKills
       , kcFactionKills
       , kcPodKills
       )
 where

import           Control.Lens.TH(makeLenses)
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as M
import           EVE.API.XML.Helpers(cachedFetcher)
import           EVE.Console(Console)
import           EVE.Static.Database.TypeIds(TypeId)
import           Text.Read(readMaybe)
import           Text.XML.Light(Element, findElements, findAttr)

data MapAPI = MapAPI {
       getMapJumpCounts :: IO (Either String (Map TypeId Word))
     , getMapKillCounts :: IO (Either String (Map TypeId KillCounts))
     }

data KillCounts = KillCounts {
       _kcShipKills    :: Word
     , _kcFactionKills :: Word
     , _kcPodKills     :: Word
     }
 deriving (Eq, Read, Show)

makeLenses ''KillCounts

emptyKillCounts :: KillCounts
emptyKillCounts = KillCounts 0 0 0

initMapAPI :: Console -> IO MapAPI
initMapAPI con =
  do getMapJumpCounts <- cachedFetcher con "map/Jumps" [] readJumpInfo
     getMapKillCounts <- cachedFetcher con "map/Kills" [] readKillInfo
     return MapAPI{..}

readJumpInfo :: Element -> Maybe (Map TypeId Word)
readJumpInfo el =
  M.fromList `fmap` sequence (map parseRow (findElements "row" el))
 where
  parseRow x =
    do solarSystemId <- readMaybe =<< findAttr "solarSystemID" x
       shipJumps     <- readMaybe =<< findAttr "shipJumps"     x
       return (fromIntegral (solarSystemId :: Word), shipJumps)

readKillInfo :: Element -> Maybe (Map TypeId KillCounts)
readKillInfo el =
  M.fromList `fmap` sequence (map parseRow (findElements "row" el))
 where
  parseRow x =
    do solarSystemId <- readMaybe =<< findAttr "solarSystemID" x
       ships         <- readMaybe =<< findAttr "shipKills"     x
       facs          <- readMaybe =<< findAttr "factionKills"  x
       pods          <- readMaybe =<< findAttr "podKills"      x
       return (fromIntegral (solarSystemId :: Word), KillCounts ships facs pods)

