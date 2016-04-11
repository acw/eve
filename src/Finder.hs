module Finder where

import Control.Lens
import Control.Lens.Criteria
import Control.Monad
import Data.List
import Data.Maybe
import Data.Ord
import EVE
import EVE.Console
import EVE.System

tradeHubs :: [String]
tradeHubs = ["Jita", "Amarr", "Rens", "Hek", "Dodixie"]
 
baseConstraint :: Criteria SolarSystem
baseConstraint = highSec `And` (Test (> 0) solarSystemIceRings)

addHubDistance :: EVE a -> SolarSystem -> Maybe (SolarSystem, String, Int)
addHubDistance eve system =
  case findRestrictedClosestMatching eve highSec system isHub of
    Nothing -> Nothing
    Just (closest, path) ->
      Just (system, view solarSystemName closest, length path + 1)
 where
  isHub = Test (`elem` tradeHubs) solarSystemName

type FinalTuple = (SolarSystem, String, Int)

score :: SolarSystem -> Word
score system =
  view (solarSystemJumpCount . non 0) system +
  (100 * (view (solarSystemKillCounts . non emptyKillCounts . kcShipKills) system))

stationOrder :: FinalTuple -> FinalTuple -> Ordering
stationOrder (ss1, _, d1) (ss2, _, d2) =
  case comparing score ss1 ss2 of
    EQ ->
      case compare d1 d2 of
        EQ -> compare (view solarSystemName ss1) (view solarSystemName ss2)
        x  -> x
    x  -> x

pad :: Int -> String -> String
pad len str = str ++ replicate (len - length str) ' '

default0 :: Num a => Maybe a -> a
default0 Nothing  = 0
default0 (Just x) = x

run :: IO ()
run =
  do eve <- initializeEVEOnline stdoutConsole
     let baseSystems = findSystemsMatching eve baseConstraint
         systemsWithHD = mapMaybe (addHubDistance eve) baseSystems
     systemsWithCounts <- forM systemsWithHD $ \ (ss, hub, dist) ->
                            do Right ss' <- addOnlineMapInfo eve ss
                               return (ss', hub, dist)
     let sorted = sortBy stationOrder systemsWithCounts
     forM_ sorted $ \ (ss, hub, distance) ->
      putStrLn (pad 20 (view solarSystemName ss) ++
                pad 10 (show (view (solarSystemJumpCount . non 0) ss)) ++
                pad 10 (show (view (solarSystemKillCounts . non emptyKillCounts . kcShipKills) ss)) ++
                pad 20 hub ++
                show distance)
