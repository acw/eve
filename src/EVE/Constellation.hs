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
import EVE.State(EVE, stDatabase)
import EVE.Static.Database(evedbConstellations)
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

toConstellationId :: EVE a -> TypeId -> Maybe ConstellationId
toConstellationId eve tid =
  case dbLookup tid (view (stDatabase . evedbConstellations) eve) of
    Nothing -> Nothing
    Just _  -> Just (CID tid)

findConstellationById :: EVE a -> ConstellationId -> Maybe Constellation
findConstellationById eve cid@(CID tid) =
  case dbLookup tid (view (stDatabase . evedbConstellations) eve) of
    Nothing -> Nothing
    Just dbc ->
      do regionId <- toRegionId eve (_conRegion dbc)
         region   <- findRegionById eve regionId
         let faction = case _conFaction dbc of
                         Nothing -> Nothing
                         Just ftid ->
                           do factionId <- toFactionId eve ftid
                              findFactionById eve factionId
         let _constellationId      = cid
             _constellationName    = _conName dbc
             _constellationRegion  = region
             _constellationFaction = faction
         return Constellation{..} 

findConstellationByName :: EVE a -> String -> Maybe Constellation
findConstellationByName eve name =
  case constellationByName (view (stDatabase.evedbConstellations) eve) name of
    Nothing  -> Nothing
    Just tid -> findConstellationById eve (CID tid)


