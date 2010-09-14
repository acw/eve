{-# LANGUAGE DeriveDataTypeable #-}
module EVE.LowLevel.DB(
         EVEDB
       , openEVEDB
       , closeEVEDB
       , disableCache
       , enableCache
       , lookupCachedOrDo
       , addCachedResponse
       --
       , updateSkillTables
       , updateCertificateTables
       , testDB
       )
 where

import Codec.Compression.BZip
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.ByteString.Lazy(ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.List
import Data.Time
import Data.Typeable
import Database.SQLite
import Network.HTTP
import Network.URI
import System.Directory
import System.Locale
import System.IO

data EVEDatabaseError = EVEDatabaseError String
  deriving (Show, Typeable)

instance Exception EVEDatabaseError

data EVEDB = EVEDB {
    dbase   :: SQLiteHandle
  , cacheOn :: MVar Bool
  }

testDB :: IO ()
testDB  = do
  exists <- doesFileExist "foo.db"
  when exists $ removeFile "foo.db"
  conn <- openConnection "foo.db"
  res  <- execStatement conn getAllTables
  case res of
    Left err -> throwIO (EVEDatabaseError err)
    Right ls -> do
      let items = map snd $ concat $ concat ls
      unless (and $ map (`elem` items) additionalTableNames) $ do
        forM_ additionalTables $ \tab -> do
          res <- defineTable conn tab
          putStrLn $ tabName tab ++ ": " ++ show res
        closeConnection conn

additionalTables :: [SQLTable]
additionalTables = [
    -- |Our request cache.
    Table {
      tabName        = "Cache"
    , tabColumns     = [
        Column {
          colName    = "request"
        , colType    = SQLChar Nothing
        , colClauses = [PrimaryKey False, Unique, IsNullable False]
        }
      , Column {
          colName    = "expireTime"
        , colType    = SQLDateTime DATETIME
        , colClauses = [IsNullable False]
        }
      , Column {
          colName    = "result"
        , colType    = SQLBlob LongBlob
        , colClauses = [IsNullable False]
        } 
      ]
    , tabConstraints = [TablePrimaryKey ["request"]]
    }
    -- tables describing skills
  , Table {
      tabName        = "SkillGroups"
    , tabColumns     = [
        Column {
          colName    = "groupID"
        , colType    = SQLInt BIG True False
        , colClauses = [IsNullable False, Unique, PrimaryKey False]
        }
      , Column {
          colName    = "groupName"
        , colType    = SQLChar Nothing
        , colClauses = [IsNullable False]
        }
      ]
    , tabConstraints = [TablePrimaryKey ["groupID"]]
    }
  , Table {
      tabName        = "Skills"
    , tabColumns     = [
        Column {
          colName    = "skillID"
        , colType    = SQLInt BIG True False
        , colClauses = [IsNullable False, Unique, PrimaryKey False]
        }
      , Column {
          colName    = "groupID"
        , colType    = SQLInt BIG True False
        , colClauses = [ForeignKey "SkillGroups" ["groupID"] cascade Nothing]
        }
      , Column {
          colName    = "name"
        , colType    = SQLChar Nothing
        , colClauses = [IsNullable False]
        }
      , Column {
          colName    = "description"
        , colType    = SQLBlob LongBlob
        , colClauses = [IsNullable False]
        }
      , Column {
          colName    = "primaryAttr"
        , colType    = SQLChar (Just 12)
        , colClauses = [IsNullable False]
        }
      , Column {
          colName    = "secondaryAttr"
        , colType    = SQLChar (Just 12)
        , colClauses = [IsNullable False]
        }
      , Column {
          colName    = "rank"
        , colType    = SQLInt TINY True False
        , colClauses = [IsNullable False]
        }
      ]
    , tabConstraints = [TablePrimaryKey ["skillID"]]
    }
  , Table {
      tabName        = "SkillReqs"
    , tabColumns     = [
        Column {
          colName    = "skillID"
        , colType    = SQLInt BIG True False
        , colClauses = [IsNullable False,
                        ForeignKey "Skills" ["skillID"] cascade Nothing]
        }
      , Column {
          colName    = "reqSkillID"
        , colType    = SQLInt BIG True False
        , colClauses = [IsNullable False, 
                        ForeignKey "Skills" ["skillID"] cascade Nothing]
        }
      , Column {
          colName    = "reqLevel"
        , colType    = SQLInt TINY True False
        , colClauses = [IsNullable False]
        }
      ]
    , tabConstraints = [TablePrimaryKey ["skillID","reqSkillID"]]
    }
  , Table {
      tabName        = "SkillBonuses"
    , tabColumns     = [
        Column {
          colName    = "skillID"
        , colType    = SQLInt BIG True False
        , colClauses = [IsNullable False,
                        ForeignKey "Skills" ["skillID"] cascade Nothing]
        }
      , Column {
          colName    = "bonus"
        , colType    = SQLChar Nothing
        , colClauses = [IsNullable False]
        }
      , Column {
          colName    = "valueInt"
        , colType    = SQLInt BIG False False
        , colClauses = []
        }
      , Column {
          colName    = "valueFloat"
        , colType    = SQLFloat Nothing Nothing
        , colClauses = []
        }
      ]
    , tabConstraints = [TablePrimaryKey ["skillID","bonus"]]
    }
    -- tables describing certificates
  , Table {
      tabName        = "CertCategories"
    , tabColumns     = [
        Column {
          colName    = "categoryName"
        , colType    = SQLChar Nothing
        , colClauses = [IsNullable False]
        }
      , Column {
          colName    = "categoryID"
        , colType    = SQLInt BIG True False
        , colClauses = [IsNullable False, Unique, PrimaryKey False]
        }
      ]
    , tabConstraints = [TablePrimaryKey ["categoryID"]]
    }
  , Table {
      tabName        = "CertClasses"
    , tabColumns     = [
        Column {
          colName    = "categoryID"
        , colType    = SQLInt BIG True False
        , colClauses = [IsNullable False,
                        ForeignKey "CertCategories" ["categoryID"]
                                   cascade Nothing]
        }
      , Column {
          colName    = "classID"
        , colType    = SQLInt BIG True False
        , colClauses = [IsNullable False, Unique, PrimaryKey False]
        }
      , Column {
          colName    = "className"
        , colType    = SQLChar Nothing
        , colClauses = [IsNullable False]
        }
      ]
    , tabConstraints = [TablePrimaryKey ["classID"]]
    }
  , Table {
      tabName        = "CertGrades"
    , tabColumns     = [
        Column {
          colName    = "certID"
        , colType    = SQLInt BIG True False
        , colClauses = [IsNullable False, Unique, PrimaryKey False]
        }
      , Column {
          colName    = "certGrade"
        , colType    = SQLInt TINY True False
        , colClauses = [IsNullable False]
        }
      , Column {
          colName    = "classID"
        , colType    = SQLInt BIG True False
        , colClauses = [IsNullable False,
                        ForeignKey "CertClasses" ["classID"] cascade Nothing]
        }
      , Column {
          colName    = "grantingCorp"
        , colType    = SQLInt BIG True False
        , colClauses = [IsNullable False]
        }
      , Column {
          colName    = "description"
        , colType    = SQLBlob LongBlob
        , colClauses = [IsNullable False]
        }
      ]
    , tabConstraints = [TablePrimaryKey ["certID"]]
    }
  , Table {
      tabName        = "CertRequiredSkills"
    , tabColumns     = [
        Column {
          colName    = "certID"
        , colType    = SQLInt BIG True False
        , colClauses = [IsNullable False,
                        ForeignKey "CertGrades" ["certID"] cascade Nothing]
        }
      , Column {
          colName    = "reqSkillID"
        , colType    = SQLInt BIG True False
        , colClauses = [IsNullable False,
                        ForeignKey "Skills" ["skillID"] cascade Nothing]
        }
      , Column {
          colName    = "reqSkillLevel"
        , colType    = SQLInt TINY True False
        , colClauses = [IsNullable False]
        }
      ]
    , tabConstraints = [TablePrimaryKey ["certID"]]
    }
  , Table {
      tabName        = "CertRequiredCerts"
    , tabColumns     = [
        Column {
          colName    = "certID"
        , colType    = SQLInt BIG True False
        , colClauses = [IsNullable False, 
                        ForeignKey "CertGrades" ["certID"] cascade Nothing]
        }
      , Column {
          colName    = "reqCert"
        , colType    = SQLInt BIG True False
        , colClauses = [IsNullable False,
                        ForeignKey "CertGrades" ["certID"] cascade Nothing]
        }
      ]
    , tabConstraints = [TablePrimaryKey ["certID","reqCert"]]
  }
  ]
 where
  cascade = [OnDelete Cascade, OnUpdate Cascade]

additionalTableNames :: [String]
additionalTableNames = map tabName additionalTables

-- |Opens an EVE library database. The boolean determines whether or not
-- this function should assume network connectivity. If it is true, then
-- if the given file does not exist, this function will create it and
-- populate it with the initial data (this takes awhile). If it is not
-- true, then it will raise an EVEDatabaseError.
--
-- If there is a problem opening the database, this function will throw
-- an EVEDatabaseError or a SQLite exception.
openEVEDB :: FilePath -> Bool -> IO EVEDB
openEVEDB loc useNetwork = do
  fileExists <- doesFileExist loc
  when ((not fileExists) && (not useNetwork)) $
    throwIO (EVEDatabaseError "Could not open file and useNetwork was False")
  unless fileExists $ downloadBaseDB loc
  conn <- openConnection loc
  res  <- execStatement conn getAllTables
  case res of
    Left err -> throwIO (EVEDatabaseError err)
    Right ls -> do
      let items = map snd $ concat $ concat ls
      unless (and $ map (`elem` items) additionalTableNames) $ do
        forM_ additionalTables $ defineTable conn
      cacheOnMV <- newMVar True
      return $ EVEDB {
        dbase   = conn
      , cacheOn = cacheOnMV
      }

-- |This version of openEVEDB is like the former, but will not utilize a
-- network. Thus, if the database does not exist, or is not properly set
-- up, this routine will return an error.
openConnectionlessEVEDB :: FilePath -> IO EVEDB
openConnectionlessEVEDB loc = do
  fileExists <- doesFileExist loc
  unless fileExists $ 

-- |Close an EVE library database. The handle will no longer be valid; using
-- it will lead to unpredictable errors.
closeEVEDB :: EVEDB -> IO ()
closeEVEDB db = closeConnection (dbase db)

downloadBaseDB :: FilePath -> IO ()
downloadBaseDB dest = do
  fhndl <- openBinaryFile dest WriteMode
  simpleHTTP dbReq >>= getResponseBody >>= BS.hPut fhndl . decompress
  hClose fhndl
 where
  dbReq :: Request ByteString
  dbReq       = mkRequest GET dbLink
  Just dbLink = parseURI "http://eve.no-ip.de/tyr104/tyr104-sqlite3-v1.db.bz2"

disableCache :: EVEDB -> IO ()
disableCache db = takeMVar (cacheOn db) >> putMVar (cacheOn db) False

enableCache :: EVEDB -> IO ()
enableCache db = takeMVar (cacheOn db) >> putMVar (cacheOn db) False

lookupCachedOrDo :: EVEDB -> String -> (String -> a) -> (IO a) -> IO a
lookupCachedOrDo db hash ifHave ifDont = do
  useCache <- readMVar $ cacheOn db
  if useCache
    then do _    <- execStatement_ (dbase db) deleteStaleCache
            mres <- execStatement  (dbase db) (lookupCachedItem hash)
            case mres of
              Left _                     -> ifDont
              Right [[("result",res):_]] -> do putStrLn "Using cache!"
                                               return $ ifHave res
              Right _                    -> ifDont
    else ifDont

addCachedResponse :: EVEDB -> String -> String -> UTCTime -> IO ()
addCachedResponse db hash response expire = do
  ignoreReturn `fmap` execStatement_ (dbase db)
                        (cacheItem hash response' formattedExpire)
  ignoreReturn `fmap` execStatement_ (dbase db) "END TRANSACTION"
 where
  formattedExpire = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" expire
  response'       = escapeQuotes response
  ignoreReturn _  = ()

escapeQuotes :: String -> String
escapeQuotes []          = []
escapeQuotes ('\'':rest) = '\"' : escapeQuotes rest
escapeQuotes (f:rest)    = f : escapeQuotes rest

-- --------------------------------------------------------------------------
-- Table updates from the EVE API
--

updateSkillTables :: EVEDB                                           ->
                     [(Integer, String)]                             ->
                     [(Integer, Integer, String, String,
                      String, String, Integer)]                      ->
                     [(Integer, Integer, Integer)]                   ->
                     [(Integer, String, Maybe Integer, Maybe Float)] ->
                     IO ()
updateSkillTables db groups skills reqs bonuses = do
  _ <- execStatement_ (dbase db) "DELETE FROM SkillBonuses;"
  _ <- execStatement_ (dbase db) "DELETE FROM SkillReqs;"
  _ <- execStatement_ (dbase db) "DELETE FROM Skills;"
  _ <- execStatement_ (dbase db) "DELETE FROM SkillGroups;"
  forM_ groups  (execStatement_ (dbase db) . insertSGroup)
  forM_ skills  (execStatement_ (dbase db) . insertSkill)
  forM_ reqs    (execStatement_ (dbase db) . insertSReq)
  forM_ bonuses (execStatement_ (dbase db) . insertSBon)
  _ <- execStatement_ (dbase db) "END TRANSACTION"
  return ()

updateCertificateTables :: EVEDB                                          ->
                           [(String, Integer)]                            ->
                           [(Integer, Integer, String)]                   ->
                           [(Integer, Integer, Integer, Integer, String)] ->
                           [(Integer, Integer, Integer)]                  ->
                           [(Integer, Integer)]                           ->
                           IO ()
updateCertificateTables db groups classes grades sreqs creqs = do
  _ <- execStatement_ (dbase db) "DELETE FROM CertCategories;"
  _ <- execStatement_ (dbase db) "DELETE FROM CertClasses;"
  _ <- execStatement_ (dbase db) "DELETE FROM CertGrades;"
  _ <- execStatement_ (dbase db) "DELETE FROM CertRequiredSkills;"
  _ <- execStatement_ (dbase db) "DELETE FROM CertRequiredCerts;"
  forM_ groups  (execStatement_ (dbase db) . insertCGroup)
  forM_ classes (execStatement_ (dbase db) . insertCClasses)
  forM_ grades  (execStatement_ (dbase db) . insertCGrades)
  forM_ sreqs   (execStatement_ (dbase db) . insertCSReq)
  forM_ creqs   (execStatement_ (dbase db) . insertCCReq)
  _ <- execStatement_ (dbase db) "END TRANSACTION"
  return ()

-- --------------------------------------------------------------------------
-- SQL Statements
--

getAllTables :: String
getAllTables = intercalate " " [
  "SELECT name FROM sqlite_master"
 ,"WHERE TYPE='table'"
 ,"ORDER BY name;"
 ]

lookupCachedItem :: String -> String
lookupCachedItem req = intercalate " " [
   "SELECT result"
  ,"FROM Cache"
  ,"WHERE request = '"++req++"' AND expireTime > datetime('now')"
  ,"ORDER BY expireTime DESC"
  ]

deleteStaleCache :: String
deleteStaleCache = intercalate " " [
   "DELETE FROM Cache"
  ,"WHERE expireTime < date('now');"
  ]

cacheItem :: String -> String -> String -> String
cacheItem req rsp expire = intercalate " " [
   "INSERT OR REPLACE INTO Cache(request,result,expireTime) VALUES"
  ,"   ('"++req++"', '"++rsp++"', '"++expire++"');"
  ]

insertSGroup :: (Integer, String) -> String
insertSGroup (gid, str) = intercalate " " [
   "INSERT OR ABORT INTO SkillGroups(groupID,groupName) VALUES"
  ,"   ("++show gid++",'"++str++"');"
  ]

insertSkill :: (Integer, Integer, String, String, String, String, Integer) ->
                String
insertSkill (sid,gid,nm,dc,pr,sn,r) = intercalate " " [
   "INSERT OR ABORT INTO Skills(skillID,groupID,name,description,"
  ,                            "primaryAttr,secondaryAttr,rank) VALUES"
  ,"   ("++show sid++","++show gid++",'"++nm++"','"++dc++"','"
  ,     pr++"','"++sn++"',"++show r++")"
  ]

insertSReq :: (Integer, Integer, Integer) -> String
insertSReq (sid,rid,lev) = intercalate " " [
   "INSERT OR ABORT INTO SkillReqs(skillID,reqSkillID,reqLevel) VALUES"
  ,"  ("++show sid++","++show rid++","++show lev++")"
  ]

insertSBon :: (Integer, String, Maybe Integer, Maybe Float) -> String
insertSBon (sid,bon,mi,mf) = intercalate " " [
   "INSERT OR ABORT INTO SkillBonuses(skillID,bonus,valueInt,valueFloat) VALUES"
  ,"  ("++show sid++",'"++bon++"',"++showm mi++","++showm mf++")"
  ]
 where
  showm Nothing  = "NULL"
  showm (Just x) = show x

insertCGroup :: (String, Integer) -> String
insertCGroup (a,b) = intercalate " " [
   "INSERT OR ABORT INTO CertCategories(categoryName,categoryID) VALUES"
  ,"  ('"++a++"',"++show b++")"
  ]

insertCClasses :: (Integer, Integer, String) -> String
insertCClasses (a,b,c) = intercalate " " [
   "INSERT OR ABORT INTO CertClasses(categoryID,classID,className) VALUES"
  ,"  ("++show a++","++show b++",'"++c++"')"
  ]

insertCGrades :: (Integer, Integer, Integer, Integer, String) -> String
insertCGrades (a,b,c,d,e) = intercalate " " [
   "INSERT OR ABORT INTO "
  ,"   CertGrades(certID,certGrade,classID,grantingCorp,description)"
  ," VALUES "
  ,"   ("++show a++","++show b++","++show c++","++show d++",'"++e++"')"
  ]

insertCSReq :: (Integer, Integer, Integer) -> String
insertCSReq (a,b,c) = intercalate " " [
   "INSERT OR ABORT INTO CertRequiredSkills(certID,reqSkillID,reqSkillLevel)"
  ,"VALUES ("++show a++","++show b++","++show c++")"
  ]

insertCCReq :: (Integer, Integer) -> String
insertCCReq (a,b) = intercalate " " [
   "INSERT OR ABORT INTO CertRequiredCerts(certID,reqCert) VALUES"
  ,"  ("++show a++","++show b++")"
  ]

