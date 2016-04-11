{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module EVE.Faction(
         Faction
       , factionId
       , factionName
       , factionDescription
       , factionHomeSystem
       , factionCorporation
       , factionMilitia
       , factionRaces
       , factionSizeFactor
       , factionStationCount
       , factionSystemCount
       , factionIcon
         --
       , toFactionId
       , findFactionById
       , findFactionByName
       )
 where

import                Control.Lens(view)
import                Control.Lens.TH(makeLenses)
import                EVE.Corporation(CorporationId, toCorporationId)
import                EVE.Graphics(IconId, toIconId)
import                EVE.Race(Race, toRaceIds, findRaceById)
import                EVE.State(EVE, stDatabase)
import                EVE.Static.Database(evedbFactionInfo)
import                EVE.Static.Database.Class(EVEDatabase(..))
import qualified      EVE.Static.Database.Faction as DBF
import                EVE.Static.Database.TypeIds(TypeId)
import {-# SOURCE #-} EVE.System(SolarSystemId, toSolarSystemId)

newtype FactionId = FID TypeId
 deriving (Eq, Ord, Read, Show)

data Faction = Faction {
      _factionId           :: FactionId
    , _factionName         :: String
    , _factionDescription  :: String
    , _factionHomeSystem   :: SolarSystemId
    , _factionCorporation  :: CorporationId
    , _factionMilitia      :: Maybe CorporationId
    , _factionRaces        :: [Race]
    , _factionSizeFactor   :: Word
    , _factionStationCount :: Word
    , _factionSystemCount  :: Word
    , _factionIcon         :: IconId
    }
 deriving (Read, Show)

instance Eq Faction where
  f == g = _factionId f == _factionId g

makeLenses ''Faction

toFactionId :: EVE a -> TypeId -> Maybe FactionId
toFactionId eve tid =
  case dbLookup tid (view (stDatabase . evedbFactionInfo) eve) of
    Nothing -> Nothing
    Just _  -> Just (FID tid)

findFactionById :: EVE a -> FactionId -> Maybe Faction
findFactionById eve (FID tid) =
  convertDBFaction eve
    (dbLookup tid (view (stDatabase . evedbFactionInfo) eve))

findFactionByName :: EVE a -> String -> Maybe Faction
findFactionByName eve name =
  convertDBFaction eve
    (DBF.factionByName (view (stDatabase . evedbFactionInfo) eve) name)

convertDBFaction :: EVE a -> Maybe DBF.Faction -> Maybe Faction
convertDBFaction _     Nothing  = Nothing
convertDBFaction eve (Just f) = 
  case (toSolarSystemId eve (DBF._facHomeSystem f),
        toRaces eve (DBF._facRaces f),
        toIconId eve (DBF._facIconId f)) of
    (Just homeSystem, Just races, Just icon) ->
       let _factionId           = FID (DBF._facId f)
           _factionName         = DBF._facName f
           _factionDescription  = DBF._facDescription f
           _factionHomeSystem   = homeSystem
           _factionCorporation  = toCorporationId (DBF._facCorporation f)
           _factionMilitia      = toCorporationId `fmap` (DBF._facMilitia f)
           _factionRaces        = races
           _factionSizeFactor   = DBF._facSizeFactor f
           _factionStationCount = DBF._facStationCount f
           _factionSystemCount  = DBF._facSystemCount f
           _factionIcon         = icon
       in Just Faction{..}
    _ -> Nothing

toRaces :: EVE a -> TypeId -> Maybe [Race]
toRaces eve tid = mapM (findRaceById eve) =<< toRaceIds eve tid
