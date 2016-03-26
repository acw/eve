{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternGuards              #-}
{-# LANGUAGE RecordWildCards            #-}
module EVE.Static.Database.TypeIds(
         TranslationLanguage
       , TypeIdDatabase
       , TypeId, GroupID, GraphicID, SoundID
       , TypeInfo(..)
       )
 where

import           Data.Bits(Bits, FiniteBits)
import qualified Data.HashMap.Strict as HM
import           Data.Map.Strict(Map)
import qualified Data.Map as M
import           Data.Scientific(toBoundedInteger)
import           Data.Text(Text, unpack)
import           Data.Yaml(FromJSON(..), Value(..), Parser, (.:), (.:?))
import           EVE.Static.Database.Class(EVEDatabase(..))
import           Text.Read(readMaybe)

data TranslationLanguage = German | English | French | Spanish | Italian
                         | Japanese | Russian | Chinese
 deriving (Eq, Ord, Read, Show)

newtype TranslatableText = TName { _unTName :: Map TranslationLanguage Text }
 deriving (Read, Show, Eq)

instance FromJSON TranslatableText where
  parseJSON obj =
    case obj of
      Object o -> TName <$> HM.foldrWithKey addTName (return M.empty) o
      _        -> fail "Non-object for translatable name."

addTName :: Text -> Value ->
            Parser (Map TranslationLanguage Text) ->
            Parser (Map TranslationLanguage Text)
addTName lang value rest =
  case (lang, value) of
    ("de", String t) -> M.insert German   t <$> rest
    ("en", String t) -> M.insert English  t <$> rest
    ("es", String t) -> M.insert Spanish  t <$> rest
    ("fr", String t) -> M.insert French   t <$> rest
    ("it", String t) -> M.insert Italian  t <$> rest
    ("ja", String t) -> M.insert Japanese t <$> rest
    ("ru", String t) -> M.insert Russian  t <$> rest
    ("zh", String t) -> M.insert Chinese  t <$> rest
    (lng,  String _) -> fail ("Unknown translation language " ++ unpack lng)
    (_,    _)        -> fail "Unknown name type."

newtype TypeId = TID Word
 deriving (Bits,Bounded,Eq,Enum,FiniteBits,Integral,Num,Ord,Read,Real,Show)

instance FromJSON TypeId where
  parseJSON obj =
    case obj of
      Number s | Just res <- toBoundedInteger s -> return (TID res)
      Number _ -> fail ("Unparseable number in type id.") 
      _ -> fail ("Non-number for type id.")

newtype GroupID = GID Word
 deriving (Bounded, Eq, Enum, Integral, Num, Ord, Read, Real, Show)

instance FromJSON GroupID where
  parseJSON obj =
    case obj of
      Number s | Just res <- toBoundedInteger s -> return (GID res)
      Number _ -> fail ("Unparseable number in group id.") 
      _ -> fail ("Non-number for group id.")

newtype GraphicID = GrID Word
 deriving (Bounded, Eq, Enum, Integral, Num, Ord, Read, Real, Show)

instance FromJSON GraphicID where
  parseJSON obj =
    case obj of
      Number s | Just res <- toBoundedInteger s -> return (GrID res)
      Number _ -> fail ("Unparseable number in graphic id.") 
      _ -> fail ("Non-number for group id.")

newtype SoundID = SID Word
 deriving (Bounded, Eq, Enum, Integral, Num, Ord, Read, Real, Show)

instance FromJSON SoundID where
  parseJSON obj =
    case obj of
      Number s | Just res <- toBoundedInteger s -> return (SID res)
      Number _ -> fail ("Unparseable number in sound id.") 
      _ -> fail ("Non-number for group id.")

data TypeInfo = TypeInfo {
       _tiId          :: TypeId
     , _tiBasePrice   :: Maybe Double
     , _tiGroupId     :: GroupID
     , _tiName        :: TranslatableText
     , _tiDescription :: Maybe TranslatableText
     , _tiPortionSize :: Word
     , _tiPublished   :: Bool
     , _tiVolume      :: Maybe Double
     , _tiMass        :: Maybe Double
     , _tiRadius      :: Maybe Double
     , _tiGraphicId   :: Maybe GraphicID
     , _tiSoundId     :: Maybe SoundID
     }
 deriving (Read, Show)

instance FromJSON TypeInfo where
  parseJSON obj =
    case obj of
      Object o ->
        do let _tiId = error "DO NOT READ THIS"
           _tiBasePrice   <- o .:? "basePrice"
           _tiGroupId     <- o .:  "groupID"
           _tiName        <- o .:  "name"
           _tiDescription <- o .:? "description"
           _tiPortionSize <- o .:  "portionSize"
           _tiRadius      <- o .:? "radius"
           _tiVolume      <- o .:? "volume"
           _tiMass        <- o .:? "mass"
           _tiPublished   <- o .:  "published"
           _tiGraphicId   <- o .:? "graphicID"
           _tiSoundId     <- o .:? "soundID"
           return TypeInfo{ .. }
      _ ->
        fail "Non-object found for type info."

newtype TypeIdDatabase = TIDDB { _tidDatabase :: Map TypeId TypeInfo }
  deriving (Read, Show)

instance FromJSON TypeIdDatabase where
  parseJSON obj =
    case obj of
      Object o -> TIDDB <$> HM.foldrWithKey addTypeInfo (return M.empty) o
      _        -> fail "Unexpected top-level type list."
   where
    addTypeInfo key value prev =
      case readMaybe (unpack key) of
        Just tid ->
          do cur <- parseJSON value
             let cur' = cur{ _tiId = TID tid }
             (M.insert (TID tid) cur') `fmap` prev
        Nothing ->
          fail "Failed to parse type id."

instance EVEDatabase TypeId TypeInfo TypeIdDatabase where
  dbRecordCount   (TIDDB db) = fromIntegral (M.size db)
  dbLookup      k (TIDDB db) = M.lookup k db
  dbKeys          (TIDDB db) = M.keys db

