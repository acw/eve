{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
module EVE.Internal.DB(
         EVEDB
       , EVEDatabaseException(..)
       , openDB
       , closeDB
       --
       , forceCacheTableExistence
       , checkCachedRequest
       , addRequestedEntry
       )
 where

import Codec.Compression.BZip         (decompress)
import Control.Concurrent             (forkIO)
import Control.Concurrent.MVar        (MVar, newEmptyMVar, takeMVar, putMVar)
import Control.Concurrent.Chan        (Chan, newChan, readChan, writeChan)
import Control.Exception              (Exception, throwIO, SomeException, catch)
import Control.Monad                  (unless)
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy           (ByteString)
import Data.ByteString.Lazy.Progress  (trackProgressString)
import Data.Typeable                  (Typeable)
import Database.SQLite                (SQLiteHandle,Value(..),SQLTable,Row,
                                       openConnection, closeConnection,
                                       execStatement, execStatement_,
                                       defineTable, tabName)
import Network.HTTP                   (simpleHTTP, Request, mkRequest)
import Network.HTTP.Base              (Response(..), RequestMethod(..))
import Network.HTTP.Headers           (HeaderName(..), findHeader)
import Network.Stream                 (ConnError)
import Network.URI                    (parseURI)
import Prelude                 hiding (catch)
import System.Directory               (getAppUserDataDirectory,
                                       createDirectoryIfMissing,
                                       doesDirectoryExist, doesFileExist)
import System.FilePath                ((</>))
import System.IO                      (openBinaryFile, hClose, IOMode(..))

import EVE.Internal.DB.Queries
import EVE.Internal.DB.Tables

--

data EVEDatabaseException = 
    EVEDatabaseUnavailable
  | EVEDownloadFailure ConnError
  | EVEDatabaseFailure String
  | EVEDatabaseTableFailure String
 deriving (Typeable, Show)

instance Exception EVEDatabaseException

--

data DatabaseRequest = 
    CloseDB (MVar ())
  | ExecStatement String (MVar (Either String [[Row Value]]))
  | ExecStatement_ String (MVar (Maybe String))
  | BuildTable SQLTable (MVar (Maybe String))

--

data EVEDB = EVEDB (Chan DatabaseRequest)

-- |Open the EVE database. if the argument is true, then openDB will attempt
-- to download the database if it doesn't currently exist. if it is false,
-- then openDB will throw an exception if the database does not exist.
openDB :: Bool -> IO EVEDB
openDB canDownload = do
  appDir   <- getAppUserDataDirectory "eveapi"
  appDirOK <- doesDirectoryExist appDir
  unless (canDownload || appDirOK) $ throwIO EVEDatabaseUnavailable
  createDirectoryIfMissing True appDir
  let dbFilename = appDir </> "EVE.db"
  dbFileOK <- doesFileExist dbFilename
  unless (canDownload || dbFileOK) $ throwIO EVEDatabaseUnavailable
  unless dbFileOK $ downloadDBFile dbFilename
  dbconn <- openConnection dbFilename
  -- we want to serialize access to the database. putting requests through a
  -- channel is one of the easier ways.
  dbRequestChan <- newChan
  _ <- forkIO $ runDatabaseConnection dbRequestChan dbconn
  return (EVEDB dbRequestChan)

-- Download the database file to the given path.
-- INVARIANTS:
--   * This function assumes the parent directory exists.
--   * This function assumes the given file does not exist.
downloadDBFile :: FilePath -> IO ()
downloadDBFile path = do
  fhndl <- openBinaryFile path WriteMode
  http <- simpleHTTP dbReq
  case http of
    Left x     -> throwIO $ EVEDownloadFailure x
    Right resp -> do
      let size = read `fmap` findHeader HdrContentLength resp
      track <- trackProgressString formatStr size handler
      track (rspBody resp) >>= BS.hPut fhndl . decompress
      hClose fhndl
 where
  dbReq :: Request ByteString
  dbReq       = mkRequest GET dbLink
  Just dbLink = parseURI "http://eve.no-ip.de/tyr104/tyr104-sqlite3-v1.db.bz2"
  formatStr   = "\rDownloading EVE DB ... %p (%R, estimated done in %T)"
  handler     = putStr

-- An endless thread that runs the database connection. Note that we want
-- to be very careful about exceptions ...
runDatabaseConnection :: Chan DatabaseRequest -> SQLiteHandle -> IO ()
runDatabaseConnection commChan hndl = catch runThread handleException
 where
  handleException :: SomeException -> IO ()
  handleException e = do
    putStrLn ("ACK! Database thread crashed with: " ++ show e)
  --
  runThread = do
    request <- readChan commChan
    case request of
      CloseDB mv           -> closeConnection hndl >> putMVar mv ()
      ExecStatement cmd mv -> execStatement hndl cmd >>= putMVar mv >> runThread
      ExecStatement_ c m   -> execStatement_ hndl c >>= putMVar m >> runThread
      BuildTable tab mv    -> defineTable   hndl tab >>= putMVar mv >> runThread

-- |Close the database connection
closeDB :: EVEDB -> IO ()
closeDB (EVEDB chan) = do
  mv <- newEmptyMVar
  writeChan chan (CloseDB mv)
  takeMVar mv

runStatement :: Chan DatabaseRequest -> String ->
                IO (Either String [[Row Value]])
runStatement chan cmd = do
  mv :: (MVar (Either String [[Row Value]])) <- newEmptyMVar
  writeChan chan (ExecStatement cmd mv)
  takeMVar mv

runStatement_ :: Chan DatabaseRequest -> String ->
                 IO ()
runStatement_ chan cmd = do
  mv <- newEmptyMVar
  writeChan chan (ExecStatement_ cmd mv)
  res <- takeMVar mv
  case res of
    Just x  -> throwIO (EVEDatabaseFailure x)
    Nothing -> return ()

defTable :: Chan DatabaseRequest -> SQLTable -> IO ()
defTable chan tab = do
  mv <- newEmptyMVar
  writeChan chan (BuildTable tab mv)
  res <- takeMVar mv
  case res of
    Nothing -> return ()
    Just x  -> throwIO (EVEDatabaseTableFailure x)

-- |Force the existence of the cache table
forceCacheTableExistence :: EVEDB -> IO ()
forceCacheTableExistence (EVEDB chan) = do
  res <- runStatement chan "SELECT name FROM sqlite_master WHERE type='table';"
  case res of
    Left err -> throwIO (EVEDatabaseFailure err)
    Right ls -> do
      let items = map (\ (Text x) -> x) $ map snd $ concat $ concat ls
      unless (tabName requestCache `elem` items) $ do
        defTable chan requestCache

-- |Find a cached request in the database, if it exists. The request cache
-- will be updated, so this has a side effect of garbage collecting the
-- database for you.
checkCachedRequest :: EVEDB -> String -> IO (Maybe String)
checkCachedRequest (EVEDB chan) reqHash = do
  runStatement_ chan cleanCache
  x <- runStatement chan (lookupCachedItem reqHash)
  case x of
    Left str                     -> throwIO (EVEDatabaseFailure str)
    Right [[(col,Text res):_]]
      | col == cacheResponseCol  -> return (Just res)
    Right _                      -> return Nothing

-- |Add an entry to the entry cache.
addRequestedEntry :: EVEDB -> String -> String -> String -> IO ()
addRequestedEntry (EVEDB chan) req resp expire =
  runStatement_ chan (insertCache req resp expire)

