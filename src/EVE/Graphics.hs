module EVE.Graphics(
         IconId
       , toIconId
       )
 where

import Control.Lens(view)
import EVE.State(EVE, stDatabase)
import EVE.Static.Database(evedbIconInfo)
import EVE.Static.Database.Class(EVEDatabase(..))
import EVE.Static.Database.Icon(_iconId)
import EVE.Static.Database.TypeIds(TypeId)

newtype IconId = IID TypeId
 deriving (Eq, Ord, Show, Read)

toIconId :: EVE a -> TypeId -> Maybe IconId
toIconId eve tid = 
  case dbLookup tid (view (stDatabase . evedbIconInfo) eve) of
    Nothing -> Nothing
    Just x  -> Just (IID (_iconId x))
