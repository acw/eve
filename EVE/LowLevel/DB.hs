{-# LANGUAGE DeriveDataTypeable #-}
module EVE.LowLevel.DB(
         EVEDB
       , openEVEDB
       , closeEVEDB
       , disableCache
       , enableCache
       , lookupCachedOrDo
       , addCachedResponse
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
  ]

additionalTableNames :: [String]
additionalTableNames = map tabName additionalTables

-- |Opens an EVE library database. If the given file does not exist, this
-- function will create it and populate it with the initial data (this
-- takes awhile). The Left value will be an error message on failure.
--
-- If there is a problem opening the database, this function will throw
-- an EVEDatabaseError or a SQLite exception..
openEVEDB :: FilePath -> IO EVEDB
openEVEDB loc = do
  fileExists <- doesFileExist loc
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
  Just dbLink = parseURI "http://eve.no-ip.de/dom103/dom103-sqlite3-v1.db.bz2"

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
