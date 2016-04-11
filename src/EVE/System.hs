{-# LANGUAGE PatternGuards   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}
module EVE.System(
         -- * The SolarSystem data structure and friends
         SolarSystem
       , SolarSystemId
       , solarSystemId
       , solarSystemName
       , solarSystemSecurity
       , solarSystemFactionId
       , solarSystemRegion
       , solarSystemConstellation
       , solarSystemIceRings
       , solarSystemAsteroidBelts
       , solarSystemNumMoons
       , solarSystemPlanetInfo
       , solarSystemIsBorder
       , solarSystemIsFringe
       , solarSystemIsCorridor
       , solarSystemIsHub
       , solarSystemIsInternational
       , solarSystemIsRegional
       , solarSystemIsConstellation
       , solarSystemJumps
         -- * Additional information available if you're online
       , addOnlineMapInfo
       , solarSystemJumpCount
       , solarSystemKillCounts
       , KillCounts, emptyKillCounts
       , kcShipKills, kcFactionKills, kcPodKills
         -- * Conversions to/from Solar Systems
       , toSolarSystemId
       , findSystemByName
       , findSystemById
       , findSystemsMatching
         -- * Solar system routing and searching
       , findPath
       , findRestrictedPath
       , findClosestMatching
       , findRestrictedClosestMatching
         -- * Useful criteria for systems
       , highSec
       , lowSec
       , nullSec
       , security
       )
 where

import           Control.Lens(view, set)
import           Control.Lens.Criteria(Criteria(..), criteriaHolds)
import           Control.Lens.TH(makeLenses)
import qualified Data.Map.Strict as M
import           Data.Maybe(mapMaybe)
import           EVE.API.XML.Map(KillCounts, emptyKillCounts,
                                 kcShipKills, kcFactionKills, kcPodKills,
                                 getMapJumpCounts, getMapKillCounts)
import           EVE.Constellation(Constellation, toConstellationId,
                                   findConstellationById)
import           EVE.Faction(Faction,toFactionId,findFactionById)
import           EVE.Region(Region,toRegionId,findRegionById)
import           EVE.State(EVE, IsOnline, stDatabase, stMapAPI)
import           EVE.Static.Database(evedbSolarSystems,evedbSystemInfo,
                                     evedbJumps)
import           EVE.Static.Database.Class(EVEDatabase(..))
import           EVE.Static.Database.Jumps(Jumps(..))
import           EVE.Static.Database.SolarSystem(_ssSecurity,_ssFactionID,
                                                 _ssRegionID,_ssConstellationID,
                                                 _ssName,_ssIsBorder,_ssIsHub,
                                                 _ssIsFringe, _ssIsCorridor,
                                                 _ssIsInternational,
                                                 _ssIsRegional,
                                                 _ssIsConstellation,
                                                 solarSystemByName)
import           EVE.Static.Database.SystemInfo(SystemInfo(..), PlanetInfo,
                                                emptySystemInfo)
import           EVE.Static.Database.TypeIds(TypeId)

newtype SolarSystemId = SSID { _ssid :: TypeId }
 deriving (Eq, Ord, Read, Show)

data SolarSystem = SolarSystem {
       _solarSystemId              :: SolarSystemId
     , _solarSystemName            :: String
     , _solarSystemSecurity        :: Double
     , _solarSystemFactionId       :: Maybe Faction
     , _solarSystemRegion          :: Region
     , _solarSystemConstellation   :: Constellation
     , _solarSystemIceRings        :: Word
     , _solarSystemAsteroidBelts   :: Word
     , _solarSystemNumMoons        :: Word
     , _solarSystemPlanetInfo      :: PlanetInfo
     , _solarSystemIsBorder        :: Bool
     , _solarSystemIsFringe        :: Bool
     , _solarSystemIsCorridor      :: Bool
     , _solarSystemIsHub           :: Bool
     , _solarSystemIsInternational :: Bool
     , _solarSystemIsRegional      :: Bool
     , _solarSystemIsConstellation :: Bool
     , _solarSystemJumpCount       :: Maybe Word
     , _solarSystemKillCounts      :: Maybe KillCounts
     , _solarSystemJumps           :: [SolarSystemId]
     }
 deriving (Read, Show)

makeLenses ''SolarSystem

toSolarSystemId :: EVE a -> TypeId -> Maybe SolarSystemId
toSolarSystemId eve tid =
  case dbLookup tid (view (stDatabase . evedbSolarSystems) eve) of
    Nothing -> Nothing
    Just _  -> Just (SSID tid)

findSystemByName :: EVE a -> String -> Maybe SolarSystem
findSystemByName eve name =
  case solarSystemByName (view (stDatabase . evedbSolarSystems) eve) name of
    Nothing  -> Nothing
    Just tid -> findSystemById eve (SSID tid)

findSystemById :: EVE a -> SolarSystemId -> Maybe SolarSystem
findSystemById eve ssid@(SSID tid) =
  case dbLookup tid (view (stDatabase . evedbSolarSystems) eve) of
    Just ss 
      | Just constellation <- convertConst (_ssConstellationID ss),
        Just region        <- convertRegion (_ssRegionID ss),
        Just faction       <- convertFaction (_ssFactionID ss) ->
          let _solarSystemId              = ssid
              _solarSystemName            = _ssName ss
              _solarSystemSecurity        = _ssSecurity ss
              _solarSystemFactionId       = faction
              _solarSystemRegion          = region
              _solarSystemConstellation   = constellation
              _solarSystemIceRings        = _siNumIceRings ssinfo
              _solarSystemAsteroidBelts   = _siNumAsteroidRings ssinfo
              _solarSystemNumMoons        = _siNumMoons ssinfo
              _solarSystemPlanetInfo      = _siPlanetInfo ssinfo
              _solarSystemIsBorder        = _ssIsBorder ss
              _solarSystemIsFringe        = _ssIsFringe ss
              _solarSystemIsCorridor      = _ssIsCorridor ss
              _solarSystemIsHub           = _ssIsHub ss
              _solarSystemIsInternational = _ssIsInternational ss
              _solarSystemIsRegional      = _ssIsRegional ss
              _solarSystemIsConstellation = _ssIsConstellation ss
              _solarSystemJumps           = map SSID ssjumps
              _solarSystemJumpCount       = Nothing
              _solarSystemKillCounts      = Nothing
          in Just SolarSystem{ .. }
    _ -> Nothing
 where
  convertConst tid'
    | Just cid <- toConstellationId eve tid',
      Just con <- findConstellationById eve cid = Just con
    | otherwise = Nothing
  convertFaction Nothing = Just Nothing
  convertFaction (Just tid')
    | Just fid <- toFactionId eve tid',
      Just fac <- findFactionById eve fid = Just (Just fac)
    | otherwise = Nothing
  convertRegion tid'
    | Just rid <- toRegionId eve tid',
      Just reg <- findRegionById eve rid = Just reg
    | otherwise = Nothing
  ssinfo  = case dbLookup tid (view (stDatabase . evedbSystemInfo) eve) of
              Nothing -> emptySystemInfo
              Just x  -> x
  ssjumps = case dbLookup tid (view (stDatabase . evedbJumps) eve) of
              Nothing -> []
              Just x  -> _toIDs x

addOnlineMapInfo :: IsOnline a =>
                    EVE a -> SolarSystem ->
                    IO (Either String SolarSystem)
addOnlineMapInfo eveo system =
  do ejumpm <- getMapJumpCounts (view stMapAPI eveo)
     ekillm <- getMapKillCounts (view stMapAPI eveo)
     case (ejumpm, ekillm) of
       (Left err, _) -> return (Left err)
       (_, Left err) -> return (Left err)
       (Right jumpm, Right killm) ->
          do let SSID tid = view solarSystemId system
                 jumps    = M.findWithDefault 0 tid jumpm
                 kills    = M.findWithDefault emptyKillCounts tid killm
             return (Right (set solarSystemJumpCount  (Just jumps) $
                            set solarSystemKillCounts (Just kills) system))

findSystemsMatching :: EVE a -> Criteria SolarSystem -> [SolarSystem]
findSystemsMatching eve criteria = filter (criteriaHolds criteria) systems
 where
  systems = mapMaybe (findSystemById eve)
              (map SSID
                 (dbKeys (view (stDatabase . evedbSolarSystems) eve)))

-- -----------------------------------------------------------------------------

findPath :: EVE a -> SolarSystem -> SolarSystem -> Maybe [SolarSystem]
findPath eve s e = snd `fmap` search eve s All (isSystem e)

findRestrictedPath :: EVE a ->
                      Criteria SolarSystem ->
                      SolarSystem -> SolarSystem ->
                      Maybe [SolarSystem]
findRestrictedPath eve c s e = snd `fmap` search eve s c (isSystem e)

findClosestMatching :: EVE a ->
                       SolarSystem -> Criteria SolarSystem ->
                       Maybe (SolarSystem, [SolarSystem])
findClosestMatching eve s c = search eve s All c

findRestrictedClosestMatching :: EVE a ->
                                 Criteria SolarSystem ->
                                 SolarSystem -> Criteria SolarSystem ->
                                 Maybe (SolarSystem, [SolarSystem])
findRestrictedClosestMatching eve c1 s c2 = search eve s c1 c2

search :: EVE a ->
          SolarSystem ->
          Criteria SolarSystem ->
          Criteria SolarSystem ->
          Maybe (SolarSystem, [SolarSystem])
search eve start okMidpoint isEndGoal = go [_solarSystemId start] initialNexts
 where
  badMidpoint = Not okMidpoint
  initialNexts =
    map (, []) (mapMaybe (findSystemById eve) (_solarSystemJumps start))
  --
  go :: [SolarSystemId] -> [(SolarSystem, [SolarSystem])] -> Maybe (SolarSystem, [SolarSystem])
  go _        [] = Nothing
  go visited ((next,path):rest)
    | _solarSystemId next `elem` visited = go visited rest
    | criteriaHolds isEndGoal next = Just (next, path)
    | criteriaHolds badMidpoint next = go ((_solarSystemId next):visited) rest
    | otherwise =
        let nexts  = mapMaybe (findSystemById eve) (_solarSystemJumps next)
            path'  = path ++ [next]
            nexts' = map (\ n -> (n, path')) nexts
        in go ((_solarSystemId next):visited) (rest ++ nexts')

-- -----------------------------------------------------------------------------

highSec :: Criteria SolarSystem
highSec = Test (\ x -> round (x * 10) >= (5 :: Int)) solarSystemSecurity

lowSec :: Criteria SolarSystem
lowSec = (Test (\ x ->
                 let sec10 = round (x * 10)
                 in (sec10 > 0) && (sec10 < (5 :: Int)))
               solarSystemSecurity)

nullSec :: Criteria SolarSystem
nullSec = Test (\ x -> round (x * 10) <= (0 :: Int)) solarSystemSecurity

security :: Double -> Criteria SolarSystem
security s = Test (\ x -> round (x * 10) == s') solarSystemSecurity
 where s' = round (s * 10) :: Int

isSystem :: SolarSystem -> Criteria SolarSystem
isSystem x = Test (== (_solarSystemId x)) solarSystemId
