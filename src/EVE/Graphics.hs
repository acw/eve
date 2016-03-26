module EVE.Graphics(
         IconId
       , toIconId
       )
 where

import Control.Lens(view)
import EVE.Static.Database(EVEDB, evedbIconInfo)
import EVE.Static.Database.Class(EVEDatabase(..))
import EVE.Static.Database.Icon(_iconId)
import EVE.Static.Database.TypeIds(TypeId)

newtype IconId = IID TypeId
 deriving (Eq, Ord, Show, Read)

toIconId :: EVEDB -> TypeId -> Maybe IconId
toIconId evedb tid = 
  case dbLookup tid (view evedbIconInfo evedb) of
    Nothing -> Nothing
    Just x  -> Just (IID (_iconId x))
