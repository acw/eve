{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module EVE.Constellation(
         -- * The constellation data structure
         Constellation
       , constellationId
       , constellationName
       , constellationRegion
       , constellationFaction
         -- * Conversion to/from constellation types
       , toConstellationId
       , findConstellationById
       , findConstellationByName
       )
 where

import Control.Lens(view)
import Control.Lens.TH(makeLenses)
import EVE.Faction(Faction, toFactionId, findFactionById)
import EVE.Region(Region, toRegionId, findRegionById)
import EVE.Static.Database(EVEDB, evedbConstellations)
import EVE.Static.Database.Class(EVEDatabase(..))
import EVE.Static.Database.Constellation(constellationByName, _conName,
                                         _conRegion, _conFaction)
import EVE.Static.Database.TypeIds(TypeId)

newtype ConstellationId = CID TypeId
 deriving (Eq, Ord, Read, Show)

data Constellation = Constellation {
       _constellationId      :: ConstellationId
     , _constellationName    :: String
     , _constellationRegion  :: Region
     , _constellationFaction :: Maybe Faction
     }
 deriving (Read, Show)

makeLenses ''Constellation

toConstellationId :: EVEDB -> TypeId -> Maybe ConstellationId
toConstellationId evedb tid =
  case dbLookup tid (view evedbConstellations evedb) of
    Nothing -> Nothing
    Just _  -> Just (CID tid)

findConstellationById :: EVEDB -> ConstellationId -> Maybe Constellation
findConstellationById evedb cid@(CID tid) =
  case dbLookup tid (view evedbConstellations evedb) of
    Nothing -> Nothing
    Just dbc ->
      do regionId <- toRegionId evedb (_conRegion dbc)
         region   <- findRegionById evedb regionId
         let faction = case _conFaction dbc of
                         Nothing -> Nothing
                         Just ftid ->
                           do factionId <- toFactionId evedb ftid
                              findFactionById evedb factionId
         let _constellationId      = cid
             _constellationName    = _conName dbc
             _constellationRegion  = region
             _constellationFaction = faction
         return Constellation{..} 

findConstellationByName :: EVEDB -> String -> Maybe Constellation
findConstellationByName evedb name =
  case constellationByName (view evedbConstellations evedb) name of
    Nothing  -> Nothing
    Just tid -> findConstellationById evedb (CID tid)


