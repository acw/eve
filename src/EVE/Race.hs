{-# LANGUAGE PatternGuards   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module EVE.Race(
         RaceId
       , Race
       , raceId
       , raceName
       , raceShortDescription
       , raceDescription
       , raceIconId
       --
       , toRaceId
       , toRaceIds
       , findRaceById
       , findRaceByName
       )
 where

import           Control.Lens(view)
import           Control.Lens.TH(makeLenses)
import           Data.Bits(popCount,countTrailingZeros,xor,shiftL)
import           EVE.Graphics(IconId, toIconId)
import           EVE.State(EVE, stDatabase)
import           EVE.Static.Database(evedbRaceInfo)
import           EVE.Static.Database.Class(EVEDatabase(..))
import qualified EVE.Static.Database.Race as DBR
import           EVE.Static.Database.TypeIds(TypeId)

newtype RaceId = RID TypeId
 deriving (Eq, Ord, Show, Read)

data Race = Race {
       _raceId               :: RaceId
     , _raceName             :: String
     , _raceShortDescription :: String
     , _raceDescription      :: Maybe String
     , _raceIconId           :: Maybe IconId
     }
 deriving (Read, Show)

makeLenses ''Race

instance Eq Race where
  r1 == r2 = _raceId r1 == _raceId r2

toRaceId :: EVE a -> TypeId -> Maybe RaceId
toRaceId eve tid =
  case dbLookup tid (view (stDatabase . evedbRaceInfo) eve) of
    Nothing  -> Nothing
    Just dbr -> return (RID (DBR._raceId dbr))

toRaceIds :: EVE a -> TypeId -> Maybe [RaceId]
toRaceIds eve tid
  | popCount tid == 0 = Just []
  | otherwise =
      let nextBit = 1 `shiftL` (countTrailingZeros tid)
          rest    = tid `xor` nextBit
      in case toRaceId eve nextBit of
           Nothing -> Nothing
           Just x  -> (x:) `fmap` toRaceIds eve rest

findRaceById :: EVE a -> RaceId -> Maybe Race
findRaceById eve (RID tid) =
  convertDBRace eve (dbLookup tid (view (stDatabase . evedbRaceInfo) eve))

findRaceByName :: EVE a -> String -> Maybe Race
findRaceByName eve name =
  convertDBRace eve
    (DBR.raceByName (view (stDatabase . evedbRaceInfo) eve) name)

convertDBRace :: EVE a -> Maybe DBR.Race -> Maybe Race
convertDBRace _     Nothing    = Nothing
convertDBRace eve (Just dbr) = Just Race {..}
 where
  _raceId = RID (DBR._raceId dbr)
  _raceName = DBR._raceName dbr
  _raceShortDescription = DBR._raceShortDescription dbr
  _raceDescription = DBR._raceDescription dbr
  _raceIconId = case DBR._raceIconId dbr of
                  Nothing -> Nothing
                  Just tid -> toIconId eve tid

