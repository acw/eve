module Finder where

import Control.Lens
import Control.Lens.Criteria
import Control.Monad
import Data.List
import Data.Maybe
import EVE.Static.Database
import EVE.Console
import EVE.System

tradeHubs :: [String]
tradeHubs = ["Jita", "Amarr", "Rens", "Hek", "Dodixie"]
 
baseConstraint :: Criteria SolarSystem
baseConstraint = highSec `And` (Test (> 0) solarSystemIceRings)

addHubDistance :: EVEDB -> SolarSystem -> Maybe (SolarSystem, String, Int)
addHubDistance evedb system =
  case findRestrictedClosestMatching evedb highSec system isHub of
    Nothing -> Nothing
    Just (closest, path) ->
      Just (system, view solarSystemName closest, length path + 1)
 where
  isHub = Test (`elem` tradeHubs) solarSystemName

type FinalTuple = (SolarSystem, String, Int)

stationOrder :: FinalTuple -> FinalTuple -> Ordering
stationOrder (ss1, _, d1) (ss2, _, d2) =
  case compare d1 d2 of
    EQ -> compare (view solarSystemName ss1) (view solarSystemName ss2)
    x  -> x

pad :: Int -> String -> String
pad len str = str ++ replicate (len - length str) ' '

run :: IO ()
run =
  do evedb <- loadStaticData stdoutConsole
     let baseSystems = findSystemsMatching evedb baseConstraint
         systemsWithHD = mapMaybe (addHubDistance evedb) baseSystems
         sorted = sortBy stationOrder systemsWithHD
     forM_ sorted $ \ (ss, hub, distance) ->
      putStrLn (pad 20 (view solarSystemName ss) ++
                pad 20 hub ++
                show distance)

