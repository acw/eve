{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
module EVE.Internal.Monad(
         EVE(..)
       , newEveState
       --
       , getApiKey
       , runIO
       , throwEVE
       , runAPIMethod
       )
 where


import Control.Applicative            (Applicative(..))
import Control.Exception              (Exception, throwIO, SomeException(..))
import Control.Monad                  (ap)
import Control.RateLimit              (rateLimitInvocation)
import Data.ByteString.Lazy.Progress  (trackProgressString)
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy.Char8     (pack, unpack)
import Data.List                      (intercalate)
import Data.Time.Format               (formatTime, readTime)
import Data.Time.LocalTime            (getCurrentTimeZone, utcToLocalTime)
import Data.Time.Units                (Second)
import Data.Typeable                  (Typeable)
import Network.HTTP                   (simpleHTTP)
import Network.HTTP.Base              (Response(..), RequestMethod(..),
                                       Request(..))
import Network.HTTP.Headers           (HeaderName(..), findHeader, Header(..))
import Network.Stream                 (ConnError(..))
import Network.URI                    (parseURI)
import System.Locale                  (defaultTimeLocale)
import Text.XML.Light.Helpers         (getElementData)
import Text.XML.Light.Input           (parseXMLDoc)
import Text.XML.Light.Output          (showTopElement)
import Text.XML.Light.Types           (Element)

import EVE.APIKey                     (APIKey)
import EVE.Internal.DB

data EveState a = EveState {
       esEVEDB  :: EVEDB
     , esAPIKey :: a
     , esRunRPC :: (String, [(String, String)]) ->
                   IO (Either SomeException Element)
     }

data EveApiException = EveApiConnectionReset
                     | EveApiConnectionClosed
                     | EveApiHttpParseError String
                     | EveApiHttpUnknownError String
                     | EveApiXmlParseError String
 deriving (Show, Typeable)

instance Exception EveApiException

-- |Initialize the EVE state
newEveState :: EVEDB -> s -> Bool -> IO (EveState s)
newEveState db k goOnline = do
  func <- if goOnline
            then rateLimitInvocation (1 :: Second) eveRPCRunner
            else return (\ _ -> fail "Type magic failed: ran RPC offline.")
  return (EveState db k func)

-- run an RPC call
eveRPCRunner :: (String, [(String, String)]) -> 
                IO (Either SomeException Element)
eveRPCRunner (procedure, args) = do
  res <- simpleHTTP (Request uri POST hdrs body)
  case res of
    Left ErrorReset     -> return (errCase EveApiConnectionReset)
    Left ErrorClosed    -> return (errCase EveApiConnectionClosed)
    Left (ErrorParse x) -> return (errCase (EveApiHttpParseError x))
    Left (ErrorMisc x)  -> return (errCase (EveApiHttpUnknownError x))
    Right resp          -> do
      let size = read `fmap` findHeader HdrContentLength resp
      track <- trackProgressString format size handler
      bstr  <- track (rspBody resp)
      case parseXMLDoc bstr of
        Nothing  -> return (errCase (EveApiXmlParseError (unpack bstr)))
        Just xml -> do
          xml `seq` putStrLn ""
          return (Right xml)
 where
  Just uri = parseURI $ "http://api.eve-online.com/" ++ procedure ++ ".xml.aspx"
  hdrs     = [Header HdrContentType "application/x-www-form-urlencoded",
              Header HdrContentLength (show $ BS.length body)]
  body     = pack $ intercalate "&" $ map (\ (a,b) -> a ++ "=" ++ b) args
  format   = "\rRunning API call " ++ procedure ++ " ... %p (%R)"
  handler  = putStr
  errCase  = Left . SomeException

data EVE s r = EVE {
    unEVE :: EveState s -> IO (r, EveState s)
  }

instance Monad (EVE s) where
  return a = EVE $ \ s -> return (a, s)
  m >>= k  = EVE $ \ s -> do
              (a, s') <- unEVE m s
              unEVE (k a) s'

instance Functor (EVE s) where
  fmap f m = EVE $ \ s -> do
              (x, s') <- unEVE m s
              return (f x, s')

instance Applicative (EVE s) where
  pure  = return
  (<*>) = ap

-- |Run an IO action in the EVE monad
runIO :: IO a -> EVE s a
runIO action = EVE $ \ s -> do
                 r <- action
                 return (r, s)

-- |Get the key while in the EVE monad
getApiKey :: APIKey t => EVE t t
getApiKey = EVE $ \ s -> return (esAPIKey s, s)

-- |Throw an exception in the EVE monad
throwEVE :: Exception e => e -> EVE s a
throwEVE = runIO . throwIO

-- |Run an API call. Will use the cacheing mechanism as possible to avoid
-- pissing people off at us.
runAPIMethod :: String -> [(String,String)] -> EVE s Element
runAPIMethod method args = EVE $ \ s -> do
  let db = esEVEDB s
  forceCacheTableExistence db
  cacheRes <- checkCachedRequest db reqHash
  case cacheRes of
    Just resp -> do
      let Just resp' = parseXMLDoc resp -- We made this string, so it's OK?
      return (resp', s)
    Nothing   -> do
      result' <- esRunRPC s (method, args)
      case result' of
        Left exc -> throwIO exc
        Right doc -> do
          myZone <- getCurrentTimeZone
          let Just cachedUntil = getElementData "cachedUntil" doc
              cachedTime       = readTime defaultTimeLocale "%F %X" cachedUntil
              expire           = utcToLocalTime myZone cachedTime
              expireStr        = formatTime defaultTimeLocale "%F %X" expire
          addRequestedEntry db reqHash (showTopElement doc) expireStr
          return (doc, s)
 where
  reqHash = method ++ "|" ++ intercalate "^" (map (uncurry (++)) args)


