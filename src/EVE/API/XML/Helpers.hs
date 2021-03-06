{-# LANGUAGE OverloadedStrings #-}
module EVE.API.XML.Helpers(
         cachedFetcher
       )
 where

import Control.Concurrent.MVar(newMVar,modifyMVar)
import Control.Exception(SomeException, handle)
import Control.Lens((^.), set)
import Data.ByteString.Char8(pack)
import Data.String(IsString(..))
import Data.Time.Clock(UTCTime, getCurrentTime)
import Data.Time.Format(defaultTimeLocale, parseTimeM)
import Data.Version
import EVE.Console(Console, logs)
import Network.Wreq(postWith, responseStatus, statusCode, responseBody,
                    defaults, header)
import Paths_EVE(version)
import Text.XML.Light(QName(..), Element(..),
                      parseXMLDoc, findChild, strContent)

data CachedItem a = NoCache | CachedItem UTCTime a

instance IsString QName where
  fromString s = QName s Nothing Nothing

fetchExpireTime :: Element -> Maybe UTCTime
fetchExpireTime el =
  do expireElement <- findChild "cachedUntil" el
     parseTimeM True defaultTimeLocale "%F %T" (strContent expireElement)

cachedFetcher :: Console -> String -> [(String,String)] ->
                 (Element -> Maybe a) ->
                 IO (IO (Either String a))
cachedFetcher con apifun args parser =
  do mv <- newMVar NoCache
     return (fetch mv)
 where
  fetch mv =
    do now <- getCurrentTime
       modifyMVar mv $ \ et ->
         case et of
           NoCache ->
             do logs con ("Running initial API call for "++show apifun++" ...")
                res <- fetchItemFromNet et
                logs con (" done.\n")
                return res
           CachedItem t _ | t < now ->
             do logs con ("Re-fetching expired entry for "++show apifun++" ...")
                res <- fetchItemFromNet et
                logs con (" done.\n")
                return res
           CachedItem _ v ->
             do logs con ("Using cached entry for " ++ show apifun ++ ".\n")
                return (et, Right v)
  --
  url  = "https://api.eve-online.com/" ++ apifun ++ ".xml.aspx"
  args' = map (\ (a,b) -> (pack a, pack b)) args
  agent = "Haskell-EVE/"++ showVersion version ++ " (http://github.com/acw/eve)"
  agentOptions = set (header "User-Agent") [fromString agent] defaults
  --
  fetchItemFromNet old =
    handle (\ e -> return (old, Left (show (e :: SomeException)))) $
      do resp <- postWith agentOptions url args'
         case resp ^. responseStatus ^. statusCode of
           200 ->
             case parseXMLDoc (resp ^. responseBody) of
               Nothing -> return (old, Left ("Couldn't read XML: " ++ show resp))
               Just xml ->
                 case fetchExpireTime xml of
                   Nothing -> return (old, Left "Couldn't find expire time")
                   Just expire ->
                     case parser xml of
                       Nothing -> return (old, Left "Parse failed")
                       Just x ->
                         return (CachedItem expire x, Right x)
           code ->
             return (old, Left ("Bad status code: " ++ show code))
