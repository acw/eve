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

import Control.Lens(view)
import Control.Lens.Criteria(Criteria(..), criteriaHolds)
import Control.Lens.TH(makeLenses)
import Data.Maybe(mapMaybe)
import EVE.Constellation(Constellation,toConstellationId,findConstellationById)
import EVE.Faction(Faction,toFactionId,findFactionById)
import EVE.Region(Region,toRegionId,findRegionById)
import EVE.Static.Database(EVEDB,evedbSolarSystems,evedbSystemInfo,evedbJumps)
import EVE.Static.Database.Class(EVEDatabase(..))
import EVE.Static.Database.Jumps(Jumps(..))
import EVE.Static.Database.SolarSystem(_ssSecurity, _ssFactionID,
                                       _ssRegionID, _ssConstellationID, _ssName,
                                       _ssIsBorder, _ssIsFringe, _ssIsCorridor,
                                       _ssIsHub, _ssIsInternational,
                                       _ssIsRegional, _ssIsConstellation,
                                       solarSystemByName)
import EVE.Static.Database.SystemInfo(SystemInfo(..),PlanetInfo,emptySystemInfo)
import EVE.Static.Database.TypeIds(TypeId)

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
     , _solarSystemJumps           :: [SolarSystemId]
     }
 deriving (Read, Show)

makeLenses ''SolarSystem

toSolarSystemId :: EVEDB -> TypeId -> Maybe SolarSystemId
toSolarSystemId evedb tid =
  case dbLookup tid (view evedbSolarSystems evedb) of
    Nothing -> Nothing
    Just _  -> Just (SSID tid)

findSystemByName :: EVEDB -> String -> Maybe SolarSystem
findSystemByName evedb name =
  case solarSystemByName (view evedbSolarSystems evedb) name of
    Nothing  -> Nothing
    Just tid -> findSystemById evedb (SSID tid)

findSystemById :: EVEDB -> SolarSystemId -> Maybe SolarSystem
findSystemById evedb ssid@(SSID tid) =
  case dbLookup tid (view evedbSolarSystems evedb) of
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
          in Just SolarSystem{ .. }
    _ -> Nothing
 where
  convertConst tid'
    | Just cid <- toConstellationId evedb tid',
      Just con <- findConstellationById evedb cid = Just con
    | otherwise = Nothing
  convertFaction Nothing = Just Nothing
  convertFaction (Just tid')
    | Just fid <- toFactionId evedb tid',
      Just fac <- findFactionById evedb fid = Just (Just fac)
    | otherwise = Nothing
  convertRegion tid'
    | Just rid <- toRegionId evedb tid',
      Just reg <- findRegionById evedb rid = Just reg
    | otherwise = Nothing
  ssinfo  = case dbLookup tid (view evedbSystemInfo evedb) of
              Nothing -> emptySystemInfo
              Just x  -> x
  ssjumps = case dbLookup tid (view evedbJumps evedb) of
              Nothing -> []
              Just x  -> _toIDs x

findSystemsMatching :: EVEDB -> Criteria SolarSystem -> [SolarSystem]
findSystemsMatching evedb criteria = filter (criteriaHolds criteria) systems
 where
  systems = mapMaybe (findSystemById evedb)
              (map SSID
                 (dbKeys (view evedbSolarSystems evedb)))

-- -----------------------------------------------------------------------------

findPath :: EVEDB -> SolarSystem -> SolarSystem -> Maybe [SolarSystem]
findPath evedb s e = snd `fmap` search evedb s All (isSystem e)

findRestrictedPath :: EVEDB ->
                      Criteria SolarSystem ->
                      SolarSystem -> SolarSystem ->
                      Maybe [SolarSystem]
findRestrictedPath evedb c s e = snd `fmap` search evedb s c (isSystem e)

findClosestMatching :: EVEDB ->
                       SolarSystem -> Criteria SolarSystem ->
                       Maybe (SolarSystem, [SolarSystem])
findClosestMatching evedb s c = search evedb s All c

findRestrictedClosestMatching :: EVEDB ->
                                 Criteria SolarSystem ->
                                 SolarSystem -> Criteria SolarSystem ->
                                 Maybe (SolarSystem, [SolarSystem])
findRestrictedClosestMatching evedb c1 s c2 = search evedb s c1 c2

search :: EVEDB ->
          SolarSystem ->
          Criteria SolarSystem ->
          Criteria SolarSystem ->
          Maybe (SolarSystem, [SolarSystem])
search evedb start okMidpoint isEndGoal = go [_solarSystemId start] initialNexts
 where
  badMidpoint = Not okMidpoint
  initialNexts =
    map (, []) (mapMaybe (findSystemById evedb) (_solarSystemJumps start))
  --
  go :: [SolarSystemId] -> [(SolarSystem, [SolarSystem])] -> Maybe (SolarSystem, [SolarSystem])
  go _        [] = Nothing
  go visited ((next,path):rest)
    | _solarSystemId next `elem` visited = go visited rest
    | criteriaHolds isEndGoal next = Just (next, path)
    | criteriaHolds badMidpoint next = go ((_solarSystemId next):visited) rest
    | otherwise =
        let nexts  = mapMaybe (findSystemById evedb) (_solarSystemJumps next)
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
