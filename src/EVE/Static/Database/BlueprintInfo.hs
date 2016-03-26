{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE RecordWildCards       #-}
module EVE.Static.Database.BlueprintInfo(
         BlueprintDatabase
       , BlueprintInfo
       )
 where

import qualified Data.HashMap.Strict as HM
import           Data.Map.Strict(Map)
import qualified Data.Map as M
import           Data.Scientific( toRealFloat)
import           Data.Text(Text, unpack)
import           Data.Yaml(FromJSON(..), Value(..), Parser, (.:))
import           EVE.Static.Database.Class(EVEDatabase(..))
import           EVE.Static.Database.TypeIds(TypeId)
import           EVE.Static.Database.Helpers(fromArray)
import           Text.Read(readMaybe)

newtype BlueprintDatabase = BPDB {_blueprintMap :: Map TypeId BlueprintInfo}
 deriving (Read, Show)

instance FromJSON BlueprintDatabase where
  parseJSON json =
    case json of
      Object o -> BPDB `fmap` HM.foldrWithKey addBlueprintInfo (return M.empty) o
      _        -> fail "Unexpected top-level blueprint list."
   where
    addBlueprintInfo key value prev =
      case readMaybe (unpack key) of
        Just bpId ->
          do cur <- parseJSON value
             (M.insert (fromIntegral (bpId :: Word)) cur) `fmap` prev
        Nothing ->
          fail "Failed to parse blueprint id (key)."

instance EVEDatabase TypeId BlueprintInfo BlueprintDatabase where
  dbRecordCount   (BPDB db) = fromIntegral (M.size db)
  dbLookup      k (BPDB db) = M.lookup k db
  dbKeys          (BPDB db) = M.keys db

data BlueprintInfo = BlueprintInfo {
       _blueprintId            :: TypeId
     , _blueprintMaxProduction :: Word
     , _blueprintActivities    :: [BlueprintActivity]
     }
 deriving (Read, Show)

instance FromJSON BlueprintInfo where
  parseJSON obj =
    case obj of
      Object o ->
        do _blueprintId            <- o .: "blueprintTypeID"
           _blueprintMaxProduction <- o .: "maxProductionLimit"
           case HM.lookup "activities" o of
             Just (Object acts) ->
               do _blueprintActivities <- mapM toActivity (HM.toList acts)
                  return BlueprintInfo{ .. }
             Just _ ->
               fail "Non-object activities field?"
             Nothing ->
               fail "Couldn't find blueprint activities"
      _ -> fail "Non-object blueprint info?"

data BlueprintActivity = ActivityCopy             CopyActivity
                       | ActivityInvention        InventionActivity
                       | ActivityManufacture      ManufactureActivity
                       | ActivityMaterialResearch MaterialResearchActivity
                       | ActivityTimeResearch     TimeResearchActivity
 deriving (Read, Show)

toActivity :: (Text, Value) -> Parser BlueprintActivity
toActivity ("copying",           v) = ActivityCopy <$> parseJSON v
toActivity ("invention",         v) = ActivityInvention <$> parseJSON v
toActivity ("manufacturing",     v) = ActivityManufacture <$> parseJSON v
toActivity ("research_material", v) = ActivityMaterialResearch <$> parseJSON v
toActivity ("research_time",     v) = ActivityTimeResearch <$> parseJSON v
toActivity (name,                _) =
  fail ("Unknown blueprint activity: " ++ unpack name)

data CopyActivity = CopyActivity {
       _copyTime :: Word
     }
 deriving (Read, Show)

instance FromJSON CopyActivity where
  parseJSON obj =
    case obj of
      Object o -> CopyActivity `fmap` (o .: "time")
      _        -> fail "Non-object copy activity."

data InventionActivity = InventionActivity {
       _inventionMaterials :: [(Word, TypeId)]
     , _inventionProducts  :: [(Double, Word, TypeId)]
     , _inventionSkills    :: [(Word, TypeId)]
     , _inventionTime      :: Word
     }
 deriving (Read, Show)

instance FromJSON InventionActivity where
  parseJSON obj =
    case obj of
      Object o ->
        do _inventionMaterials <- fromArray         "materials" o parseMaterial
           _inventionProducts  <- fromArray         "products"  o parseIProduct
           _inventionSkills    <- fromArray         "skills"    o parseSkill
           _inventionTime      <- o .: "time"
           return InventionActivity{ .. }
      _ ->
        fail "Non-object invention activity?"

parseMaterial :: Value -> Parser (Word, TypeId)
parseMaterial obj =
  case obj of
    Object o -> do quant  <- o .: "quantity"
                   typeId <- o .: "typeID"
                   return (quant, typeId)
    _ -> fail "Couldn't parse material (it's not an object?)"

parseIProduct :: Value -> Parser (Double, Word, TypeId)
parseIProduct obj =
  case obj of
    Object o -> do (quant, typeId) <- parseMaterial obj
                   case HM.lookup "probability" o of
                     Just (Number x) ->
                       return (toRealFloat x, quant, typeId)
                     Just _ ->
                       fail "Non-numeric probability?"
                     Nothing ->
                       return (1.0, quant, typeId)
    _ -> fail "Non-object invention product."

parseSkill :: Value -> Parser (Word, TypeId)
parseSkill obj =
  case obj of
    Object o -> do level  <- o .: "level"
                   typeId <- o .: "typeID"
                   return (level, typeId)
    _ -> fail "Couldn't parse skill (it's not an object?)"

data ManufactureActivity = ManufactureActivity {
       _manufactureMaterials :: [(Word, TypeId)]
     , _manufactureProducts  :: [(Word, TypeId)]
     , _manufactureSkills    :: [(Word, TypeId)]
     , _manufactureTime      :: Word
     }
 deriving (Read, Show)

instance FromJSON ManufactureActivity where
  parseJSON obj =
    case obj of
      Object o ->
        do _manufactureMaterials <- fromArray "materials" o parseMaterial
           _manufactureProducts  <- fromArray "products"  o parseMaterial
           _manufactureSkills    <- fromArray "skills"    o parseSkill
           _manufactureTime      <- o .: "time"
           return ManufactureActivity{ .. }
      _ ->
        fail "Non-object manufacture activity."

data MaterialResearchActivity = MaterialResearchActivity {
       _mresearchTime :: Word
     }
 deriving (Read, Show)

instance FromJSON MaterialResearchActivity where
  parseJSON obj =
    case obj of
      Object o -> MaterialResearchActivity `fmap` (o .: "time")
      _        -> fail "Non-object material research activity."

data TimeResearchActivity = TimeResearchActivity {
       _tresearchTime :: Word
     }
 deriving (Read, Show)

instance FromJSON TimeResearchActivity where
  parseJSON obj =
    case obj of
      Object o -> TimeResearchActivity `fmap` (o .: "time")
      _        -> fail "Non-object time research activity"
