module EVE.Corporation(
         CorporationId
       , toCorporationId
       )
 where

import EVE.Static.Database.TypeIds(TypeId)

newtype CorporationId = CID TypeId
 deriving (Eq, Ord, Read, Show)

toCorporationId :: TypeId -> CorporationId
toCorporationId = CID

