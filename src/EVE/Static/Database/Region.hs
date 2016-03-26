{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
module EVE.Static.Database.Region(
         RegionDatabase
       , Region(..)
       , regionByName
       , loadRegions
       )
 where

import           Control.Exception(throwIO)
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as M
import           Data.Maybe(mapMaybe)
import           Database.SQLite(SQLiteHandle, execStatement, Row, Value(..))
import           EVE.Static.Database.Class
import           EVE.Static.Database.Helpers(lookupIdent, lookupName)
import           EVE.Static.Database.TypeIds(TypeId)

data RegionDatabase = RDB {
       _region  :: Map TypeId Region
     , _nameMap :: Map String TypeId
     }
 deriving (Read, Show)

instance EVEDatabase TypeId Region RegionDatabase where
  dbRecordCount   (RDB m _) = fromIntegral (M.size m)
  dbLookup      k (RDB m _) = M.lookup k m
  dbKeys          (RDB m _) = M.keys m

regionByName :: RegionDatabase -> String -> Maybe TypeId
regionByName (RDB _ m) n = M.lookup n m

data Region = Region {
       _regId      :: TypeId
     , _regName    :: String
     , _regFaction :: Maybe TypeId
     }
  deriving (Read, Show)

loadRegions :: SQLiteHandle -> IO RegionDatabase
loadRegions sql =
  do result <- execStatement sql "SELECT * FROM mapRegions"
     case result of
       Left err ->
         throwIO (userError (show err))
       Right rows ->
         let base = M.fromList (mapMaybe processRow (concat rows))
             name = M.foldr addReverseEntry M.empty base
         in return (RDB base name)
 where addReverseEntry v m = M.insert (_regName v) (_regId v) m

processRow :: Row Value -> Maybe (TypeId, Region)
processRow row =
  do _regId          <- lookupIdent row "regionID"
     _regName        <- lookupName  row "regionName"
     let _regFaction =  lookupIdent row "factionID"
     return (_regId, Region{ .. })

