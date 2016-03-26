{-# LANGUAGE OverloadedStrings #-}
module EVE.API.XML.Helpers(
         cachedFetcher
       )
 where

import Control.Concurrent.MVar(newMVar,modifyMVar)
import Control.Exception(SomeException, handle)
import Control.Lens((^.))
import Data.ByteString.Char8(pack)
import Data.String(IsString(..))
import Data.Time.Clock(UTCTime, getCurrentTime)
import Data.Time.Format(defaultTimeLocale, parseTimeM)
import Network.Wreq(post, responseStatus, statusCode, responseBody)
import Text.XML.Light(QName(..), Element(..),
                      parseXMLDoc, findChild, strContent)

data CachedItem a = NoCache | CachedItem UTCTime a

instance IsString QName where
  fromString s = QName s Nothing Nothing

fetchExpireTime :: Element -> Maybe UTCTime
fetchExpireTime el =
  do expireElement <- findChild "cachedUntil" el
     parseTimeM True defaultTimeLocale "%F %T" (strContent expireElement)

cachedFetcher :: String -> [(String,String)] ->
                 (Element -> Maybe a) ->
                 IO (IO (Either String a))
cachedFetcher apifun args parser =
  do mv <- newMVar NoCache
     return (fetch mv)
 where
  fetch mv =
    do now <- getCurrentTime
       modifyMVar mv $ \ et ->
         case et of
           NoCache -> fetchItemFromNet et
           CachedItem t _ | t < now -> fetchItemFromNet et
           CachedItem _ v -> return (et, Right v)
  --
  url  = "https://api.eve-online.com/" ++ apifun ++ ".xml.aspx"
  args' = map (\ (a,b) -> (pack a, pack b)) args
  --
  fetchItemFromNet old =
    handle (\ e -> return (old, Left (show (e :: SomeException)))) $
      do resp <- post url args'
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
