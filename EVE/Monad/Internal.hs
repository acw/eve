module EVE.Monad.Internal where

import Control.Applicative
import Control.Arrow(first)
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Digest.Pure.SHA
import Data.List
import Data.Maybe
import Data.Time
import Network.HTTP
import Network.Stream
import Network.URI
import System.Locale
import Text.XML.Light
import Text.XML.Light.Helpers

import EVE.Errors
import EVE.LowLevel.DB

class APIKey k where
  keyToArgs :: k -> [(String, String)]

-- |A limited API key, suitable for sharing with your friends.
data LimitedAPIKey = LAPIK { limUserID  :: String, limAPIKey  :: String }

instance APIKey LimitedAPIKey where
  keyToArgs (LAPIK x y) = [("userID", x), ("apiKey", y)]

-- |A full API key, which you should probably keep more secret.
data FullAPIKey    = FAPIK { fullUserID :: String, fullAPIKey :: String }

instance APIKey FullAPIKey where
  keyToArgs (FAPIK x y) = [("userID", x), ("apiKey", y)]

-- |The type of a character ID.
newtype CharacterID      = CharID Integer deriving (Eq)

instance Read CharacterID where
  readsPrec p s = map (first CharID) (readsPrec p s)

instance Show CharacterID where
  show (CharID x) = "char:" ++ show x

-- |A generic reference ID.
newtype RefID            = RID    Integer deriving (Show)

data EveState a = EveState {
    esEveDB     :: EVEDB
  , esAPIKey    :: a
  , esCharacter :: Maybe CharacterID
  }

newtype EVE constatus r = EVE { 
    unEVE :: EveState constatus -> IO (r, EveState constatus) 
  }

newtype Offline = Offline ()

instance Monad (EVE a) where
  return a = EVE $ \ s -> return (a, s)
  m >>= k  = EVE $ \ s -> do
              (a, s') <- unEVE m s
              unEVE (k a) s'

instance Functor (EVE a) where
  fmap f m = EVE $ \ s -> do
              (x, s') <- unEVE m s
              return (f x, s')

instance Applicative (EVE a) where
  pure  = return
  (<*>) = ap

runIO :: IO a -> EVE k a
runIO action = EVE $ \ s -> do
                r <- action
                return (r, s) 

getCharacter :: EVE k (Maybe CharacterID)
getCharacter = EVE $ \ s -> return (esCharacter s, s)

getKey :: APIKey k => EVE k k
getKey  = EVE $ \ s -> return (esAPIKey s, s)

getDB :: APIKey k => EVE k EVEDB
getDB  = EVE $ \ s -> return (esEveDB s, s)

setCharacter :: APIKey k => CharacterID -> EVE k ()
setCharacter c = EVE $ \ s -> return ((), s { esCharacter = Just c })

throwEVE :: Exception e => e -> EVE k a
throwEVE = runIO . throwIO

-- Low-level API stuff

walkableRequest :: APIKey k =>
                   String -> String ->
                   (Element -> Maybe a) ->
                   Maybe RefID ->
                   EVE k a
walkableRequest proc name finish ref = do
  extendedRequest extra proc finish
 where extra = case ref of
                 Nothing        -> []
                 Just (RID rid) -> [(name, show rid)]

standardRequest :: APIKey k =>
                   String -> (Element -> Maybe a) ->
                   EVE k a
standardRequest = extendedRequest []

extendedRequest :: APIKey k =>
                   [(String, String)] -> String ->
                   (Element -> Maybe a) ->
                   EVE k a
extendedRequest extras proc finish = do
  key <- getKey
  mchar <- getCharacter
  case mchar of
    Just (CharID cid) -> do
      let args = keyToArgs key ++ extras ++ [("characterID", show cid)]
      runRequest proc args finish
    Nothing           ->
      throwEVE MustSetCharacterFirst

runRequest :: APIKey k => -- This prereq to guarantee online-ness
              String -> [(String, String)] ->
              (Element -> Maybe a) ->
              EVE k a
runRequest procedure args finishProcessing = do
  db <- getDB
  runIO $ lookupCachedOrDo db reqHash parseResult (runRequest' db)
 where
  parseResult str =
    case parseXMLDoc str of
      Nothing -> throw (XMLParseError str)
      Just r  -> case finishProcessing r of
                   Just result -> result
                   Nothing     -> throw (EVEParseError r)
  --
  runRequest' db = do
    res <- simpleHTTP req
    case res of
      Left ErrorReset     -> throwIO  ConnectionReset
      Left ErrorClosed    -> throwIO  ConnectionClosed
      Left (ErrorParse x) -> throwIO (HTTPParseError x)
      Left (ErrorMisc  x) -> throwIO (UnknownError x)
      Right resp          -> do
        let bod = rspBody resp
        case parseXMLDoc bod of
          Nothing         -> throwIO  (XMLParseError bod)
          Just xml        -> do
            let expireTime = cachedUntil xml
                mresult    = finishProcessing xml
            addCachedResponse db reqHash bod expireTime
            case mresult of
              Just result -> return result
              Nothing     -> throwIO (EVEParseError xml)
  --
  Just uri = parseURI $ "http://api.eve-online.com/" ++ procedure ++ ".xml.aspx"
  req      = Request uri POST hdrs body
  hdrs     = [Header HdrContentType "application/x-www-form-urlencoded",
              Header HdrContentLength (show $ length body)]
  body     = intercalate "&" $ map (\ (a,b) -> a ++ "=" ++ b) args
  reqHash  = showDigest $ sha512 $ BSC.pack $ show req ++ body

cachedUntil :: Element -> UTCTime
cachedUntil xml = fromMaybe zeroHour $ do
  str <- getElementStringContent "cachedUntil" xml
  parseTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" str

zeroHour :: UTCTime
zeroHour = UTCTime (toEnum 0) (toEnum 0)

boolify :: Integer -> Bool
boolify 0 = False
boolify 1 = True
boolify _ = throw (EVETypeConversionError "boolify")


