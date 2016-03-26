{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
module EVE.Static.Database.SystemInfo(
         SystemInfoDatabase
       , SystemInfo(..), emptySystemInfo
       , PlanetInfo(..), emptyPlanetInfo
       )
 where

import qualified Data.HashMap.Strict as HM
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as M
import           Data.Text(unpack)
import           Data.Yaml(FromJSON(..), Value(..), (.:))
import           EVE.Static.Database.Class(EVEDatabase(..))
import           EVE.Static.Database.TypeIds(TypeId)
import           Text.Read(readMaybe)

newtype SystemInfoDatabase = PIDB { _planetInfoMap :: Map TypeId SystemInfo }
 deriving (Read, Show)

instance FromJSON SystemInfoDatabase where
  parseJSON json =
    case json of
      Object o -> PIDB `fmap` HM.foldrWithKey addSystemInfo (return M.empty) o
      _        -> fail "Unexpected top-level blueprint list."
   where
    addSystemInfo key value prev =
      do cur <- parseJSON value
         case readMaybe (unpack key) of
           Just key' -> (M.insert (fromIntegral (key' :: Word)) cur) `fmap` prev
           Nothing   -> fail "Couldn't read type id."

instance EVEDatabase TypeId SystemInfo SystemInfoDatabase where
  dbRecordCount   (PIDB db) = fromIntegral (M.size db)
  dbLookup      k (PIDB db) = M.lookup k db
  dbKeys          (PIDB db) = M.keys db

data SystemInfo = SystemInfo {
       _siNumIceRings      :: Word
     , _siNumAsteroidRings :: Word
     , _siNumMoons         :: Word
     , _siPlanetInfo       :: PlanetInfo 
     }
 deriving (Read, Show)

emptySystemInfo :: SystemInfo
emptySystemInfo = SystemInfo 0 0 0 emptyPlanetInfo

instance FromJSON SystemInfo where
  parseJSON obj =
    case obj of
      Object o ->
        do let _siName = error "DO NOT READ THIS NAME YET"
           _siNumIceRings      <- o .: "ice"
           _siNumAsteroidRings <- o .: "belts"
           _siNumMoons         <- o .: "moons"
           _siPlanetInfo       <- o .: "planets"
           return SystemInfo{ .. }
      _ -> fail "Non-object system info"

data PlanetInfo = PlanetInfo {
       _piNumShattered :: Word
     , _piNumStorm     :: Word
     , _piNumIce       :: Word
     , _piNumLava      :: Word
     , _piNumGas       :: Word
     , _piNumBarren    :: Word
     , _piNumPlasma    :: Word
     , _piNumTemperate :: Word
     , _piNumOcean     :: Word
     }
 deriving (Read, Show)

emptyPlanetInfo :: PlanetInfo
emptyPlanetInfo = PlanetInfo 0 0 0 0 0 0 0 0 0

instance FromJSON PlanetInfo where
  parseJSON obj =
    case obj of
      Object o ->
        do _piNumShattered <- o .: "shattered"
           _piNumStorm     <- o .: "storm"
           _piNumIce       <- o .: "ice"
           _piNumLava      <- o .: "lava"
           _piNumGas       <- o .: "gas"
           _piNumBarren    <- o .: "barren"
           _piNumPlasma    <- o .: "plasma"
           _piNumTemperate <- o .: "temperate"
           _piNumOcean     <- o .: "ocean"
           return PlanetInfo{..}
      _ -> fail "Non-object planet info"

