{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
module EVE.Static.Database.Constellation(
         ConstellationDatabase
       , Constellation(..)
       , constellationByName
       , loadConstellations
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

data ConstellationDatabase = CDB {
       _constellationMap :: Map TypeId Constellation
     , _nameMap          :: Map String TypeId
     }
 deriving (Read, Show)

instance EVEDatabase TypeId Constellation ConstellationDatabase where
  dbRecordCount   (CDB m _) = fromIntegral (M.size m)
  dbLookup      k (CDB m _) = M.lookup k m
  dbKeys          (CDB m _) = M.keys m

constellationByName :: ConstellationDatabase -> String -> Maybe TypeId
constellationByName (CDB _ m) n = M.lookup n m

data Constellation = Constellation {
       _conId      :: TypeId
     , _conName    :: String
     , _conRegion  :: TypeId
     , _conFaction :: Maybe TypeId
     }
 deriving (Read, Show)

loadConstellations :: SQLiteHandle -> IO ConstellationDatabase
loadConstellations sql =
  do result <- execStatement sql "SELECT * FROM mapConstellations"
     case result of
       Left err ->
         throwIO (userError (show err))
       Right rows ->
         let base = M.fromList (mapMaybe processRow (concat rows))
             name = M.foldr addReverseEntry M.empty base
         in return (CDB base name)
 where addReverseEntry v m = M.insert (_conName v) (_conId v) m

processRow :: Row Value -> Maybe (TypeId, Constellation)
processRow row =
  do _conId          <- lookupIdent row "constellationID"
     _conName        <- lookupName  row "constellationName"
     _conRegion      <- lookupIdent row "regionID"
     let _conFaction =  lookupIdent row "factionID"
     return (_conId, Constellation{ .. })

