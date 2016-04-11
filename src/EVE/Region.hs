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
import EVE.State(EVE, stDatabase)
import EVE.Static.Database(evedbRegions)
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

toRegionId :: EVE a -> TypeId -> Maybe RegionId
toRegionId eve tid =
  case dbLookup tid (view (stDatabase . evedbRegions) eve) of
    Nothing -> Nothing
    Just _  -> Just (RID tid)

findRegionById :: EVE a -> RegionId -> Maybe Region
findRegionById eve rid@(RID tid) =
  case dbLookup tid (view (stDatabase . evedbRegions) eve) of
    Nothing -> Nothing
    Just dbr ->
      let _regionId      = rid
          _regionName    = _regName dbr
          _regionFaction = case _regFaction dbr of
                             Nothing -> Nothing
                             Just ftid ->
                               do factionId <- toFactionId eve ftid
                                  findFactionById eve factionId
      in Just Region{..}

findRegionByName :: EVE a -> String -> Maybe Region
findRegionByName eve name =
  case regionByName (view (stDatabase . evedbRegions) eve) name of
    Nothing  -> Nothing
    Just tid -> findRegionById eve (RID tid)

