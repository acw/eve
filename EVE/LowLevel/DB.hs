module EVE.LowLevel.DB(
         EVEDB
       , openEVEDB
       , downloadBaseDB
       )
 where

import Codec.Compression.BZip
import Control.Monad
import Data.ByteString.Lazy(ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.List
import Database.SQLite
import Foreign.Ptr
import Network.HTTP
import Network.URI
import System.Directory
import System.IO

data EVEDB = EVEDB {
    db :: SQLiteHandle
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
-- You can optionally add a printer that will give you some output
-- regarding status. This function will call it with only complete
-- status lines, as if it were putStrLn
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
        return $ Right EVEDB {
          db = conn
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

-- --------------------------------------------------------------------------
-- SQL Statements
--

getAllTables :: String
getAllTables = intercalate " " [
  "SELECT name FROM sqlite_master"
 ,"WHERE TYPE='table'"
 ,"ORDER BY name;"
 ]
