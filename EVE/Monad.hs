{-# LANGUAGE RankNTypes #-}
module EVE.Monad (
         EVE
       , runEVEWithKey
       , runEVEOffline
       , LimitedAPIKey, mkLimitedAPIKey
       , FullAPIKey, mkFullAPIKey
       )
 where

import Control.Exception
import System.Directory
import System.FilePath

import EVE.LowLevel.DB
import EVE.Monad.Internal

-- |Make a limited API key from the given user ID and API key. You can get
-- this information from this web page: https://www.eveonline.com/login.asp
mkLimitedAPIKey :: String -> String -> LimitedAPIKey
mkLimitedAPIKey = LAPIK

-- |Make a full API key from the given user ID and API key. You can get
-- this information from this web page: https://www.eveonline.com/login.asp
mkFullAPIKey :: String -> String -> FullAPIKey
mkFullAPIKey = FAPIK

-- |Run an EVE action using the given API keys. EVE API keys can be gotten
-- from the eve-online main site.
runEVEWithKey :: APIKey k => k -> EVE k a -> IO a
runEVEWithKey k m = bracket (openDB True) closeEVEDB $ \ db ->
  fst `fmap` unEVE m (EveState db k Nothing)

openDB :: IO EVEDB
openDB = do 
  basedir <- getAppUserDataDirectory "eveapi"
  createDirectoryIfMissing True basedir
  openEVEDB (basedir </> "eveapi.db")

-- |Run an EVE action in offline mode.
runEVEOffline :: EVE Offline a -> IO a
runEVEOffline m = bracket (openDB False) closeEVEDB $ \ db ->
  fst `fmap` unEve m (EveState db k Nothing)
  


