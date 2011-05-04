module EVE.Internal.DB.Queries(
         cleanCache
       , lookupCachedItem
       , insertCache
       )
 where

import Data.List(intercalate)
import Database.SQLite(tabName)
import EVE.Internal.DB.Tables

cleanCache :: String
cleanCache = intercalate " " [
    "DELETE FROM " ++ tabName requestCache
  , "WHERE " ++ cacheExpireCol ++ " < datetime('now','localtime');"
  ]

lookupCachedItem :: String -> String
lookupCachedItem hash = intercalate " " [
    "SELECT " ++ cacheResponseCol
  , "FROM " ++ tabName requestCache 
  , "WHERE " ++ cacheRequestCol ++ " = '" ++ hash ++ "';" 
  ]

insertCache :: String -> String -> String -> String
insertCache req rsp expi = intercalate " " [
    "INSERT OR REPLACE INTO " ++ tabName requestCache
  , "(" ++ cacheRequestCol ++ ","
        ++ cacheResponseCol ++ ","
        ++ cacheExpireCol ++ ")"
  , "VALUES(" ++ sanitizeStr req ++ ","
              ++ sanitizeStr rsp ++ ","
              ++ sanitizeStr expi ++ ");"
  ]

sanitizeStr :: String -> String
sanitizeStr  = strify . sanitize

sanitize :: String -> String
sanitize []          = []
sanitize ('\'':rest) = '\"' : sanitize rest
sanitize (f:rest)    = f    : sanitize rest

strify :: String -> String
strify x = "'" ++ x ++ "'"
