{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
module EVE.Static.Database.Icon(
         IconDatabase
       , Icon(..)
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

newtype IconDatabase = IDB (Map TypeId Icon)
 deriving (Read, Show)

instance EVEDatabase TypeId Icon IconDatabase where
  dbRecordCount   (IDB m) = fromIntegral (M.size m)
  dbLookup      k (IDB m) = M.lookup k m
  dbKeys          (IDB m) = M.keys m

instance FromJSON IconDatabase where
  parseJSON obj =
    case obj of
      Object o -> IDB `fmap` HM.foldrWithKey addIconInfo (return M.empty) o
      _        -> fail "Unexpected top-level icon list."
   where
    addIconInfo key value prev =
      do cur <- parseJSON value
         case readMaybe (unpack key) of
           Just key' ->
             let tid  = fromIntegral (key' :: Word)
                 cur' = cur{ _iconId = tid }
             in M.insert tid cur' `fmap` prev
           Nothing   -> fail "Couldn't read type id."

data Icon = Icon {
       _iconId               :: TypeId
     , _iconDescription      :: Maybe String
     , _iconFile             :: String
     }
 deriving (Read, Show)

instance FromJSON Icon where
  parseJSON obj =
    case obj of
      Object o ->
        do let _iconId = error "Do not read this race id!"
           _iconDescription      <- o .:? "description"
           _iconFile             <- o .:  "iconFile"
           return Icon{..}
      _ -> fail "Non-object icon info"
