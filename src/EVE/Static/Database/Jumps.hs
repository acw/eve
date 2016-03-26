{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
module EVE.Static.Database.Jumps(
         JumpDatabase
       , Jumps(..)
       , loadJumps
       )
 where

import           Control.Exception(throwIO)
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as M
import           Database.SQLite(SQLiteHandle, execStatement, Row, Value(..))
import           EVE.Static.Database.Class(EVEDatabase(..))
import           EVE.Static.Database.Helpers(lookupIdent)
import           EVE.Static.Database.TypeIds(TypeId)

newtype JumpDatabase = JDB { _jumpMap :: Map TypeId Jumps }
 deriving (Read, Show)

instance EVEDatabase TypeId Jumps JumpDatabase where
  dbRecordCount   (JDB m) = fromIntegral (M.size m)
  dbLookup      k (JDB m) = M.lookup k m
  dbKeys          (JDB m) = M.keys m

data Jumps = Jumps { _fromID :: TypeId, _toIDs :: [TypeId] }
 deriving (Read, Show)

loadJumps :: SQLiteHandle -> IO JumpDatabase
loadJumps sql =
  do result <- execStatement sql ("SELECT fromSolarSystemID, toSolarSystemID "
                               ++ "FROM mapSolarSystemJumps")
     case result of
       Left err ->
         throwIO (userError (show err))
       Right rows ->
         return (JDB (foldr processRow M.empty (concat rows)))

processRow :: Row Value -> Map TypeId Jumps -> Map TypeId Jumps
processRow row acc =
  case rowdata of
    Nothing         -> acc
    Just (key, val) -> M.insertWith combine key val acc
 where
  combine a b | _fromID a == _fromID b = a{ _toIDs = _toIDs a ++ _toIDs b }
              | otherwise              = error "Bad Jumps join."
  rowdata =
    do _fromID <- lookupIdent row "fromSolarSystemID"
       _toID   <- lookupIdent row "toSolarSystemID"
       let _toIDs = [_toID]
       return (_fromID, Jumps{ .. })
