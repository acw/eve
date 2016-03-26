{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
module EVE.Static.Database.Faction(
         FactionDatabase
       , Faction(..)
       , factionByName
       )
 where

import qualified Data.HashMap.Strict as HM
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as M
import           Data.Text(unpack)
import           Data.Yaml(FromJSON(..), Value(..), (.:), (.:?))
import           EVE.Static.Database.Class(EVEDatabase(..))
import           EVE.Static.Database.TypeIds(TypeId)
import           Text.Read(readMaybe)

newtype FactionDatabase = FDB (Map TypeId Faction)
 deriving (Read, Show)

instance EVEDatabase TypeId Faction FactionDatabase where
  dbRecordCount   (FDB m) = fromIntegral (M.size m)
  dbLookup      k (FDB m) = M.lookup k m
  dbKeys          (FDB m) = M.keys m

factionByName :: FactionDatabase -> String -> Maybe Faction
factionByName (FDB m) name = M.foldr findFaction Nothing m
 where
  findFaction _   res@(Just _) = res
  findFaction fac Nothing
    | _facName fac == name = Just fac
    | otherwise            = Nothing

instance FromJSON FactionDatabase where
  parseJSON obj =
    case obj of
      Object o -> FDB `fmap` HM.foldrWithKey addFactionInfo (return M.empty) o
      _        -> fail "Unexpected top-level faction list."
   where
    addFactionInfo key value prev =
      do cur <- parseJSON value
         case readMaybe (unpack key) of
           Just key' ->
             let tid  = fromIntegral (key' :: Word)
                 cur' = cur{ _facId = tid }
             in M.insert tid cur' `fmap` prev
           Nothing   -> fail "Couldn't read type id."

data Faction = Faction {
       _facId           :: TypeId
     , _facName         :: String
     , _facDescription  :: String
     , _facHomeSystem   :: TypeId
     , _facCorporation  :: TypeId
     , _facMilitia      :: Maybe TypeId
     , _facRaces        :: TypeId
     , _facSizeFactor   :: Word
     , _facStationCount :: Word
     , _facSystemCount  :: Word
     , _facIconId       :: TypeId
     }
 deriving (Read, Show)

instance FromJSON Faction where
  parseJSON obj =
    case obj of
      Object o ->
        do let _facId = error "Do not read this faction id!"
           _facName         <- o .:  "factionName"
           _facDescription  <- o .:  "description"
           _facHomeSystem   <- o .:  "solarSystemID"
           _facCorporation  <- o .:  "corporationID"
           _facMilitia      <- o .:? "militiaCorporationID"
           _facRaces        <- o .:  "raceIDs"
           _facSizeFactor   <- o .:  "sizeFactor"
           _facStationCount <- o .:  "stationCount"
           _facSystemCount  <- o .:  "stationSystemCount"
           _facIconId       <- o .:  "iconID"
           return Faction{..}
      _ -> fail "Non-object faction info"
