{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module EVE.Static.Database(
       -- * A nice little console abstraction
         Console
       , conFlush, conPut
       , stdoutConsole, stderrConsole, nullConsole
       , logs
       -- * The EVE database and its lenses
       , EVEDB
       , evedbTypeIds
       , evedbBlueprintInfo
       , evedbFactionInfo
       , evedbIconInfo
       , evedbRaceInfo
       , evedbSystemInfo
       , evedbSolarSystems
       , evedbConstellations
       , evedbRegions
       , evedbJumps
       -- * Database loading
       , loadStaticData
       )
 where

import Control.Exception(throwIO)
import Control.Lens.TH(makeLenses)
import Data.Yaml(FromJSON(..), decodeFileEither)
import Database.SQLite(SQLiteHandle, openReadonlyConnection)
import EVE.Static.Database.BlueprintInfo
import EVE.Static.Database.Class
import EVE.Static.Database.Constellation
import EVE.Static.Database.Faction
import EVE.Static.Database.Icon
import EVE.Static.Database.Jumps
import EVE.Static.Database.Race
import EVE.Static.Database.Region
import EVE.Static.Database.SolarSystem
import EVE.Static.Database.SystemInfo
import EVE.Static.Database.TypeIds
import Paths_EVE(getDataFileName)
import System.IO(hPutStr, hFlush, stderr, stdout)

data EVEDB = EVEDB {
       _evedbTypeIds        :: TypeIdDatabase
     , _evedbBlueprintInfo  :: BlueprintDatabase
     , _evedbConstellations :: ConstellationDatabase
     , _evedbFactionInfo    :: FactionDatabase
     , _evedbIconInfo       :: IconDatabase
     , _evedbRaceInfo       :: RaceDatabase
     , _evedbRegions        :: RegionDatabase
     , _evedbSystemInfo     :: SystemInfoDatabase
     , _evedbSolarSystems   :: SolarSystemDatabase
     , _evedbJumps          :: JumpDatabase
     }

makeLenses ''EVEDB

data Console = Console {
       _conPut   :: String -> IO ()
     , _conFlush :: IO ()
     }

makeLenses ''Console

logs :: Console -> String -> IO ()
logs c s = _conPut c s >> _conFlush c

loadYamlDatabase :: (FromJSON a, EVEDatabase k v a) => Console -> String -> IO a
loadYamlDatabase con name =
  do logs con ("Loading " ++ name ++ " database ... ")
     path <- getDataFileName (name ++ ".yaml")
     mdecode <- decodeFileEither path
     case mdecode of
       Right res ->
         do logs con (show (dbRecordCount res) ++ " records loaded.\n")
            return res
       Left err  ->
         do let msg = "Failed to decode " ++ name ++ " database: " ++ show err
            throwIO (userError msg)

loadSQLDatabase :: EVEDatabase k v db =>
                   Console -> String ->
                   SQLiteHandle -> (SQLiteHandle -> IO db) ->
                   IO db
loadSQLDatabase con name sql builder =
  do logs con ("Loading " ++ name ++ " database ... ")
     res <- builder sql
     logs con (show (dbRecordCount res) ++ " records loaded.\n")
     return res

stdoutConsole :: Console
stdoutConsole = Console (hPutStr stdout) (hFlush stdout)

stderrConsole :: Console
stderrConsole = Console (hPutStr stderr) (hFlush stderr)

nullConsole :: Console
nullConsole = Console (const (return ())) (return ())

loadStaticData :: Console -> IO EVEDB
loadStaticData con =
  do sql <- openReadonlyConnection =<< getDataFileName "universeDataDx.db"
     _evedbBlueprintInfo  <- loadYamlDatabase con "blueprints"
     _evedbFactionInfo    <- loadYamlDatabase con "factionInfo"
     _evedbIconInfo       <- loadYamlDatabase con "iconIDs"
     _evedbRaceInfo       <- loadYamlDatabase con "raceInfo"
     _evedbTypeIds        <- loadYamlDatabase con "typeIDs"
     _evedbSystemInfo     <- loadYamlDatabase con "systemInfo"
     _evedbSolarSystems   <- loadSQLDatabase  con "solar system"
                                              sql loadSolarSystems
     _evedbConstellations <- loadSQLDatabase  con "constellations"
                                              sql loadConstellations
     _evedbRegions        <- loadSQLDatabase  con "regions"
                                              sql loadRegions
     _evedbJumps          <- loadSQLDatabase  con "jumps"
                                              sql loadJumps
     return EVEDB{..}
