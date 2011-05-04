module EVE.Connection(
         module EVE.APIKey
       , EVE
       --
       , Offline, Online
       , runEVEWithKey
       , runEVEWithoutKey
       , runEVEOffline
       --
       , EVEDatabaseException(..)
       --
       , tranquilityServerStatus
       )
 where

import Control.Exception     (bracket)
import Control.Monad         (unless)
import Data.Maybe            (listToMaybe)
import Data.Word             (Word64)
import EVE.APIKey            (APIKey, FullAPIKey, LimitedAPIKey)
import EVE.Internal.DB       (openDB, closeDB, EVEDatabaseException(..))
import EVE.Internal.Monad    (EVE(..), newEveState, runAPIMethod)
import Text.XML.Light        (Element)
import Text.XML.Light.Helpers(getElementData)

-- |Phantom type for the EVE monad running offline
newtype Offline = RunOffline Int

-- |Phantom type for the EVE monad for running online, but without a key. This
-- is useful if you don't have a key, don't want to give over a key, or don't
-- want to run any of the access-controlled API functions that require a key.
newtype Online  = RunOnline Int

-- |A little useful typeclass for restricting the EVE monad to only those
-- situations in which it is online.
class CanGoOnline t;
instance CanGoOnline FullAPIKey;
instance CanGoOnline LimitedAPIKey;
instance CanGoOnline Online;

-- |Run an EVE computation while offline. If the EVE database does not exist
-- on your system, this will throw an exception (EVEDatabaseException).
runEVEOffline :: EVE Offline a -> IO a
runEVEOffline m = bracket (openDB False) closeDB $ \ db -> do
  state <- newEveState db (RunOffline 42) False
  fst `fmap` unEVE m state

-- |Run an EVE computation while online, but without a given key. If the EVE
-- database does not exist and there is a problem in downloading it, this
-- will throw an exception (EVEDatabaseException).
runEVEWithoutKey :: EVE Online a -> IO a
runEVEWithoutKey m = bracket (openDB True) closeDB $ \ db -> do
  state <- newEveState db (RunOnline 42) True
  fst `fmap` unEVE m state

-- |Run an EVE computation with the given API key. If the EVE database does
-- not exist and there is a problem in downloading it, this will throw an
-- exception (EVEDatabaseException).
runEVEWithKey :: APIKey k => k -> EVE k a -> IO a
runEVEWithKey k m = bracket (openDB True) closeDB $ \ db -> do
  state <- newEveState db k True
  fst `fmap` unEVE m state

-- * Useful baseline functions

-- |Whether or not a server is online, and if so, how many people are using
-- it.
data ServerStatus = Offline | Online Word64 | Other Element
  deriving (Show)

tranquilityServerStatus :: CanGoOnline s => EVE s ServerStatus
tranquilityServerStatus = do
  xml <- runAPIMethod "Server/ServerStatus" []
  case parseResult xml of
    Just players -> return (Online players)
    Nothing      -> return Offline
 where
  parseResult xml = do
    open    <- mread =<< getElementData "serverOpen" xml
    unless open $ fail "Server not open" -- translated into Nothing
    players <- mread =<< getElementData "onlinePlayers" xml
    return players

mread :: Read a => String -> Maybe a
mread  = fmap fst . listToMaybe . reads

