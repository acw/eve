module EVE.LowLevel.DB(
         EVEDB
       , openEVEDB
       , downloadBaseDB
       , disableCache
       , enableCache
       , lookupCachedOrDo
       , addCachedResponse
       )
 where

import Codec.Compression.BZip
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Data.ByteString.Lazy(ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.List
import Data.Time
import Database.SQLite
import Foreign.Ptr
import Network.HTTP
import Network.URI
import System.Directory
import System.Locale
import System.IO

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
openEVEDB :: FilePath -> IO (Either String EVEDB)
openEVEDB path = catch openConn (\ e -> return $ Left $ show e)
 where
  openConn :: IO (Either String EVEDB)
  openConn = do
    fileExists <- doesFileExist path
    unless fileExists $ downloadBaseDB path
    conn <- openConnection path
    res <- execStatement conn getAllTables
    case res of
      Left err -> do
        closeConnection conn
        return $ Left $ show err
      Right ls -> do
        let items = map snd $ concat $ concat ls
        unless (and $ map (`elem` items) additionalTableNames) $ do
          forM_ additionalTables $ defineTable conn
        cacheOnMV <- newMVar True
        forkIO $ forever $ do
          execStatement_ conn deleteStaleCache
          threadDelay $ 10 * 60 * 1000000 -- ten minutes
        return $ Right EVEDB {
          dbase   = conn
        , cacheOn = cacheOnMV
        }

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
    then do res <- execStatement (dbase db) (lookupCachedItem hash)
            case res of
              Left _                     -> ifDont
              Right [[("result",res):_]] -> return $ ifHave res
              Right _                    -> ifDont
    else ifDont

addCachedResponse :: EVEDB -> String -> String -> UTCTime -> IO ()
addCachedResponse db hash response expire = do
  res <- execStatement_ (dbase db) (cacheItem hash response' formattedExpire)
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
cacheItem req rsp exp = intercalate " " [
   "INSERT OR REPLACE INTO Cache(request,result,expireTime) VALUES"
  ,"   ('"++req++"', '"++rsp++"', '"++exp++"');"
  ]
