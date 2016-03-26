{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module EVE.Region(
         Region
       , regionId
       , regionName
       , regionFaction
         --
       , toRegionId
       , findRegionById
       , findRegionByName
       )
 where

import Control.Lens(view)
import Control.Lens.TH(makeLenses)
import EVE.Faction(Faction, toFactionId, findFactionById)
import EVE.Static.Database(EVEDB, evedbRegions)
import EVE.Static.Database.Class(EVEDatabase(..))
import EVE.Static.Database.Region(regionByName, _regName, _regFaction)
import EVE.Static.Database.TypeIds(TypeId)

newtype RegionId = RID TypeId
 deriving (Eq, Ord, Read, Show)

data Region = Region {
       _regionId      :: RegionId
     , _regionName    :: String
     , _regionFaction :: Maybe Faction
     }
 deriving (Read, Show)

makeLenses ''Region

toRegionId :: EVEDB -> TypeId -> Maybe RegionId
toRegionId evedb tid =
  case dbLookup tid (view evedbRegions evedb) of
    Nothing -> Nothing
    Just _  -> Just (RID tid)

findRegionById :: EVEDB -> RegionId -> Maybe Region
findRegionById evedb rid@(RID tid) =
  case dbLookup tid (view evedbRegions evedb) of
    Nothing -> Nothing
    Just dbr ->
      let _regionId      = rid
          _regionName    = _regName dbr
          _regionFaction = case _regFaction dbr of
                             Nothing -> Nothing
                             Just ftid ->
                               do factionId <- toFactionId evedb ftid
                                  findFactionById evedb factionId
      in Just Region{..}

findRegionByName :: EVEDB -> String -> Maybe Region
findRegionByName evedb name =
  case regionByName (view evedbRegions evedb) name of
    Nothing  -> Nothing
    Just tid -> findRegionById evedb (RID tid)

