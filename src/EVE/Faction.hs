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
import                EVE.Corporation
import                EVE.Graphics
import                EVE.Race
import                EVE.Static.Database(EVEDB, evedbFactionInfo)
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

toFactionId :: EVEDB -> TypeId -> Maybe FactionId
toFactionId evedb tid =
  case dbLookup tid (view evedbFactionInfo evedb) of
    Nothing -> Nothing
    Just _  -> Just (FID tid)

findFactionById :: EVEDB -> FactionId -> Maybe Faction
findFactionById evedb (FID tid) =
  convertDBFaction evedb (dbLookup tid (view evedbFactionInfo evedb))

findFactionByName :: EVEDB -> String -> Maybe Faction
findFactionByName evedb name =
  convertDBFaction evedb (DBF.factionByName (view evedbFactionInfo evedb) name)

convertDBFaction :: EVEDB -> Maybe DBF.Faction -> Maybe Faction
convertDBFaction _     Nothing  = Nothing
convertDBFaction evedb (Just f) = 
  case (toSolarSystemId evedb (DBF._facHomeSystem f),
        toRaces evedb (DBF._facRaces f),
        toIconId evedb (DBF._facIconId f)) of
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

toRaces :: EVEDB -> TypeId -> Maybe [Race]
toRaces evedb tid = mapM (findRaceById evedb) =<< toRaceIds evedb tid
