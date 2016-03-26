{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module EVE.Static.Database.Class(
         EVEDatabase(..)
       )
 where

class EVEDatabase key val db | db -> val, db -> key where
  dbRecordCount :: db -> Word
  dbLookup      :: key -> db -> Maybe val
  dbKeys        :: db -> [key]
