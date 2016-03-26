{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
module EVE.Static.Database.Race(
         RaceDatabase
       , Race(..)
       , raceByName
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

newtype RaceDatabase = RDB (Map TypeId Race)
 deriving (Read, Show)

-- this database is small enough that we don't bother with a reverse map
raceByName :: RaceDatabase -> String -> Maybe Race
raceByName (RDB m) name = M.foldr findName Nothing m
 where
  findName _    res@(Just _) = res
  findName race Nothing
    | _raceName race == name = Just race
    | otherwise              = Nothing

instance EVEDatabase TypeId Race RaceDatabase where
  dbRecordCount   (RDB m) = fromIntegral (M.size m)
  dbLookup      k (RDB m) = M.lookup k m
  dbKeys          (RDB m) = M.keys m

instance FromJSON RaceDatabase where
  parseJSON obj =
    case obj of
      Object o -> RDB `fmap` HM.foldrWithKey addRaceInfo (return M.empty) o
      _        -> fail "Unexpected top-level race list."
   where
    addRaceInfo key value prev =
      do cur <- parseJSON value
         case readMaybe (unpack key) of
           Just key' ->
             let tid  = fromIntegral (key' :: Word)
                 cur' = cur{ _raceId = tid }
             in M.insert tid cur' `fmap` prev
           Nothing   -> fail "Couldn't read type id."

data Race = Race {
       _raceId               :: TypeId
     , _raceName             :: String
     , _raceShortDescription :: String
     , _raceDescription      :: Maybe String
     , _raceIconId           :: Maybe TypeId
     }
 deriving (Read, Show)

instance FromJSON Race where
  parseJSON obj =
    case obj of
      Object o ->
        do let _raceId = error "Do not read this race id!"
           _raceName             <- o .:  "raceName"
           _raceShortDescription <- o .:  "shortDescription"
           _raceDescription      <- o .:? "description"
           _raceIconId           <- o .:? "iconID"
           return Race{..}
      _ -> fail "Non-object race info"
