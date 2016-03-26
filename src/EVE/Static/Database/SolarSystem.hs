{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
module EVE.Static.Database.SolarSystem(
         SolarSystemDatabase
       , SolarSystem(..)
       , solarSystemByName
       , loadSolarSystems 
       )
 where

import           Control.Exception(throwIO)
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as M
import           Data.Maybe(mapMaybe)
import           Database.SQLite(SQLiteHandle, execStatement, Row, Value(..))
import           EVE.Static.Database.Class
import           EVE.Static.Database.Helpers(lookupIdent, lookupDouble,
                                             lookupName, lookupBool)
import           EVE.Static.Database.TypeIds(TypeId)

data SolarSystemDatabase = SSDB {
       _solarSystemMap :: Map TypeId SolarSystem
     , _nameMap        :: Map String TypeId
     }
 deriving (Read, Show)

instance EVEDatabase TypeId SolarSystem SolarSystemDatabase where
  dbRecordCount   (SSDB m _) = fromIntegral (M.size m)
  dbLookup      k (SSDB m _) = M.lookup k m
  dbKeys          (SSDB m _) = M.keys m

solarSystemByName :: SolarSystemDatabase -> String -> Maybe TypeId
solarSystemByName (SSDB _ db) n = M.lookup n db

data SolarSystem = SolarSystem {
       _ssID              :: TypeId
     , _ssSecurity        :: Double
     , _ssFactionID       :: Maybe TypeId
     , _ssRegionID        :: TypeId
     , _ssConstellationID :: TypeId
     , _ssName            :: String
     , _ssIsBorder        :: Bool
     , _ssIsFringe        :: Bool
     , _ssIsCorridor      :: Bool
     , _ssIsHub           :: Bool
     , _ssIsInternational :: Bool
     , _ssIsRegional      :: Bool
     , _ssIsConstellation :: Bool
     }
 deriving (Read, Show)

loadSolarSystems :: SQLiteHandle -> IO SolarSystemDatabase
loadSolarSystems sql =
  do result <- execStatement sql "SELECT * FROM mapSolarSystems"
     case result of
       Left err   ->
         throwIO (userError (show err))
       Right rows ->
         let base = M.fromList (mapMaybe processRow (concat rows))
             name = M.foldr addReverseEntry M.empty base
         in return (SSDB base name)
 where addReverseEntry v m = M.insert (_ssName v) (_ssID v) m

processRow :: Row Value -> Maybe (TypeId, SolarSystem)
processRow row =
  do _ssID              <- lookupIdent  row  "solarSystemID"
     _ssSecurity        <- lookupDouble row "security"
     let _ssFactionID   =  lookupIdent  row "factionID"
     _ssRegionID        <- lookupIdent  row "regionID"
     _ssConstellationID <- lookupIdent  row "constellationID"
     _ssName            <- lookupName   row "solarSystemName"
     _ssIsBorder        <- lookupBool   row "border"
     _ssIsFringe        <- lookupBool   row "fringe"
     _ssIsCorridor      <- lookupBool   row "corridor"
     _ssIsHub           <- lookupBool   row "hub"
     _ssIsInternational <- lookupBool   row "international"
     _ssIsRegional      <- lookupBool   row "regional"
     _ssIsConstellation <- lookupBool   row "constellation"
     return (_ssID, SolarSystem{ .. })
