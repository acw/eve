module EVE.Map( System, Constellation, Region
              , findRegionByName, findRegionById
              , regionName
              , findConstellationByName, findConstellationById
              , constellationName, constellationRegion
              , findSystemByName, findSystemById
              , systemName, systemConstellation, systemRegion, systemSecurity
              , systemJumps
              , plotRoute, connectedTo, allSystems
              )
 where

import Data.Int          (Int64)
import Data.Maybe        (isJust)
import qualified Data.Set          as Set
import Database.SQLite   (Value(..),Row)
import EVE.Internal.Monad(EVE,EveApiException(..),runDBQuery,throwEVE,
                          findIntColumn, findStringColumn, findDoubleColumn)

import Debug.Trace

--

data Region = Region !String !Int64

instance Eq Region where
  (==) (Region _ a) (Region _ b) = a == b
  (/=) (Region _ a) (Region _ b) = a /= b

instance Ord Region where
  compare (Region _ a) (Region _ b) = compare a b

instance Show Region where
  show (Region a _ ) = "region:" ++ a

regionName :: Region -> String
regionName (Region n _) = n

findRegionByName :: String -> EVE s (Maybe Region)
findRegionByName s = do
  checkRes <- runDBQuery query
  case checkRes of
    Left err     -> throwEVE (QueryFailure $ "Looking up region id: " ++ err)
    Right [[x]]  -> return $ Just $ Region s $ findIntColumn "regionId" x
    Right _      -> return Nothing
 where
  query = unlines [ "SELECT regionId "
                   ,"FROM mapRegions "
                   ,"WHERE regionName = '" ++ s ++ "';"]

findRegionById :: Int64 -> EVE s (Maybe Region)
findRegionById id = do
  checkRes <- runDBQuery query
  case checkRes of
    Left err     -> throwEVE (QueryFailure $ "Looking up region name: " ++ err)
    Right [[x]]  -> return $ Just $ Region (findStringColumn "regionName" x) id
    Right _      -> return Nothing
 where
  query = unlines [ "SELECT regionName "
                   ,"FROM mapRegions "
                   ,"WHERE regionId = " ++ show id ++ ";"]

--

data Constellation = Constellation !String !Region !Int64

instance Eq Constellation where
  (==) (Constellation _ _ a) (Constellation _ _ b) = a == b
  (/=) (Constellation _ _ a) (Constellation _ _ b) = a /= b

instance Ord Constellation where
  compare (Constellation _ _ a) (Constellation _ _ b) = compare a b

instance Show Constellation where
  show (Constellation a _ _) = "constellation:" ++ a

constellationName :: Constellation -> String
constellationName (Constellation n _ _) = n

constellationRegion :: Constellation -> Region
constellationRegion (Constellation _ r _) = r

findConstellationByName :: String -> EVE s (Maybe Constellation)
findConstellationByName s = do
  checkRes <- runDBQuery query
  case checkRes of
    Left err     -> throwEVE (QueryFailure $ "Looking up const name: " ++ err)
    Right [[x]]  -> return $ Just $
                     Constellation s
                      (Region (findStringColumn "regionName" x)
                              (findIntColumn "regoinID" x))
                      (findIntColumn "constellationID" x)
    Right _      -> return Nothing
 where
  query = unlines [ "SELECT mapRegions.regionId,regionName,constellationId "
                   ,"FROM mapRegions INNER JOIN mapConstellations "
                   ,"ON mapRegions.regionId = mapConstellations.regionID "
                   ,"WHERE constellationName = '" ++ s ++ "';"]

findConstellationById :: Int64 -> EVE s (Maybe Constellation)
findConstellationById id = do
  checkRes <- runDBQuery query
  case checkRes of
    Left err     -> throwEVE (QueryFailure $ "Looking up const. id: " ++ err)
    Right [[x]]  -> return $ Just $
                     Constellation
                      (findStringColumn "constellationName" x)
                      (Region (findStringColumn "regionName" x)
                              (findIntColumn "regoinId" x))
                      id
    Right _      -> return Nothing
 where
  query = unlines [ "SELECT mapRegions.regionId,regionName,constellationName "
                   ,"FROM mapRegions INNER JOIN mapConstellations "
                   ,"ON mapRegions.regionId = mapConstellations.regionID "
                   ,"WHERE constellationId = " ++ show id ++ ";"]

--

data System = System !String !Constellation !Int64 !Double

instance Eq System where
  (==) (System _ _ a _) (System _ _ b _) = a == b
  (/=) (System _ _ a _) (System _ _ b _) = a /= b

instance Ord System where
  compare (System _ _ a _) (System _ _ b _) = compare a b

instance Show System where
  show (System a _ _ _) = "system:" ++ a

systemName :: System -> String
systemName (System n _ _ _) = n

systemConstellation :: System -> Constellation
systemConstellation (System _ c _ _) = c

systemRegion :: System -> Region
systemRegion = constellationRegion . systemConstellation

systemSecurity :: System -> Double
systemSecurity (System _ _ _ s) = s

buildStandardSystem :: Row Value -> System
buildStandardSystem x = System
  (findStringColumn "solarSystemName" x)
  (Constellation
    (findStringColumn "constellationName" x)
    (Region (findStringColumn "regionName" x)
            (findIntColumn "regionID" x))
    (findIntColumn "constellationID" x))
  (findIntColumn "solarSystemID" x)
  (findDoubleColumn "security" x)

findSystemByName :: String -> EVE s (Maybe System)
findSystemByName s = do
  checkRes <- runDBQuery query
  case checkRes of
    Left err     -> throwEVE (QueryFailure $ "Looking up system name: " ++ err)
    Right [[x]]  -> return $ Just $ buildStandardSystem x
    Right _      -> return Nothing
 where
  query = unlines [ "SELECT mapRegions.regionName, "
                  , "       mapRegions.regionId, "
                  , "       mapConstellations.constellationName, "
                  , "       mapConstellations.constellationId, "
                  , "       mapSolarSystems.solarSystemId, "
                  , "       mapSolarSystems.solarSystemName, "
                  , "       mapSolarSystems.security "
                  , "FROM mapRegions, mapConstellations, mapSolarSystems "
                  , "WHERE mapRegions.regionId = mapSolarSystems.regionId "
                  , "  AND mapConstellations.constellationId = "
                  , "      mapSolarSystems.constellationId "
                  , "  AND mapSolarSystems.solarSystemName = '" ++ s ++ "';"
                  ]

-- I should probably simpify these ... 
findSystemById :: Int64 -> EVE s (Maybe System)
findSystemById id = do
  checkRes <- runDBQuery query
  case checkRes of
    Left err     -> throwEVE (QueryFailure $ "Looking up system id: " ++ err)
    Right [[x]]  -> return $ Just $ buildStandardSystem x
    Right _      -> return Nothing
 where
  query = unlines [ "SELECT mapRegions.regionName, "
                  , "       mapRegions.regionId, "
                  , "       mapConstellations.constellationName, "
                  , "       mapConstellations.constellationId, "
                  , "       mapSolarSystems.solarSystemId, "
                  , "       mapSolarSystems.solarSystemName, "
                  , "       mapSolarSystems.security "
                  , "FROM mapRegions, mapConstellations, mapSolarSystems "
                  , "WHERE mapRegions.regionId = mapSolarSystems.regionId "
                  , "  AND mapConstellations.constellationId = "
                  , "      mapSolarSystems.constellationId "
                  , "  AND mapSolarSystems.solarSystemId = " ++ show id ++ ";"
                  ]

-- |Returns all systems a ship can jump to from the given system.
systemJumps :: System -> EVE s [System]
systemJumps (System _ _ id _) = do
  checkRes <- runDBQuery query
  case checkRes of
    Left err   -> throwEVE (QueryFailure $ "Looking up system jumps: " ++ err)
    Right [xs] -> return (map buildStandardSystem xs)
    Right _    -> throwEVE (QueryFailure "Bad state in systemJumps.")
 where
  query = unlines [ "SELECT R.regionName, R.regionId, "
                  , "       C.constellationName, C.constellationId, "
                  , "       S.solarSystemId, S.solarSystemName, S.security "
                  , "FROM mapRegions as R, "
                  , "     mapConstellations as C, "
                  , "     mapSolarSystems as S, "
                  , "     mapSolarSystemJumps as J"
                  , "WHERE R.regionId = J.toRegionID "
                  , "  AND C.constellationId = J.toConstellationID "
                  , "  AND S.solarSystemId = J.toSolarSystemID "
                  , "  AND J.fromSolarSystemId = " ++ show id ++ ";"
                  ]

plotRoute :: System -> System -> (Maybe (System -> EVE s Bool)) ->
             EVE s (Maybe [System])
plotRoute start end mfilter = trace ("plotting " ++ show start ++ " -> " ++ show end) $
  runSearch Set.empty [(start,[start])]
 where
  runSearch _    []              = return Nothing
  runSearch done ((x,path):rest) = do
    skip <- shouldSkip done x
    case skip of
      True              -> runSearch done rest
      False | x == end  -> return $ Just $ reverse path
            | otherwise -> do jumps <- systemJumps x
                              let states = map (\ x -> (x,x:path)) jumps
                              runSearch (x `Set.insert` done) (rest ++ states)
  --
  shouldSkip done x
    | x `Set.member` done = return True
    | otherwise           = case mfilter of
                              Just f  -> not `fmap` f x
                              Nothing -> return False

connectedTo :: System -> System -> EVE s Bool
connectedTo a b = isJust `fmap` plotRoute a b Nothing

allSystems :: EVE s [System]
allSystems = do
  checkRes <- runDBQuery query
  case checkRes of
    Left err    -> throwEVE (QueryFailure $ "Looking up system name: " ++ err)
    Right [xs]  -> return $ map buildStandardSystem xs
    Right _     -> throwEVE (QueryFailure $ "Bad state in allSystems")
 where
  query = unlines [ "SELECT mapRegions.regionName, "
                  , "       mapRegions.regionId, "
                  , "       mapConstellations.constellationName, "
                  , "       mapConstellations.constellationId, "
                  , "       mapSolarSystems.solarSystemId, "
                  , "       mapSolarSystems.solarSystemName, "
                  , "       mapSolarSystems.security "
                  , "FROM mapRegions, mapConstellations, mapSolarSystems "
                  , "WHERE mapRegions.regionId = mapSolarSystems.regionId "
                  , "  AND mapConstellations.constellationId = "
                  , "      mapSolarSystems.constellationId "
                  ]


