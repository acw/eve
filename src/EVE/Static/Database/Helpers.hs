{-# LANGUAGE PatternGuards #-}
module EVE.Static.Database.Helpers(
         fromArray
       , lookupIdent
       , lookupDouble
       , lookupName
       , lookupBool
       )
 where

import qualified Data.HashMap.Strict as HM
import           Data.Text(Text, unpack)
import           Data.Traversable(forM)
import qualified Data.Vector as V
import           Data.Yaml(Object, Parser)
import qualified Data.Yaml as Y
import           Database.SQLite(Row)
import qualified Database.SQLite as Q
import           EVE.Static.Database.TypeIds(TypeId)

fromArray :: Text -> Object -> (Y.Value -> Parser a) -> Parser [a]
fromArray fname obj constructor =
  case HM.lookup fname obj of
    Just (Y.Array a) -> V.toList `fmap` forM a constructor
    Just _ ->
      fail ("Field '" ++ unpack fname ++ "' is not an array!")
    Nothing ->
      return []

lookupIdent :: Row Q.Value -> String -> Maybe TypeId
lookupIdent row name =
  case lookup name row of
    Just (Q.Int x) -> Just (fromIntegral x)
    _              -> Nothing

lookupDouble :: Row Q.Value -> String -> Maybe Double
lookupDouble row name =
  case lookup name row of
    Just (Q.Double x) -> Just x
    _                 -> Nothing

lookupName :: Row Q.Value -> String -> Maybe String
lookupName row name =
  case lookup name row of
    Just (Q.Text x) -> Just x
    _               -> Nothing

lookupBool :: Row Q.Value -> String -> Maybe Bool
lookupBool row name =
  case lookup name row of
    Just (Q.Int x) -> Just (x /= 0)
    _              -> Nothing
