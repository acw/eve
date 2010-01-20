module EVE.LowLevel.API
{-
       (
         characterList
       , charAccountBalances
       , charAssetList
       , characterSheet
       , charFactionalWarfareStats
       , charIndustryJobs
       , charKillLogs
       , charMarketOrders
       , charMedals
       , charSkillInTraining
       , charSkillQueue
       , charStandings
       , charWalletJournal
       , charWalletTransactions
       , charMailMessages
       , charNotifications
       , charMailingLists
       , corpAccountBalances
       , corpAssetList
       , corpContainerLog
       , corporationSheet
       , corpFactionalWarfareStats
       , corpIndustryJobs
       , corpKillLogs
       , corpMarketOrders
       , corpMedals
       , corpMemberMedals
       , corpMemberSecurity
       , corpMemberSecurityLog
       , corpMemberTracking
       , corpPOSDetails
       , corpPOSList
       , corpShareholders
       , corpStandings
       , corpTitles
       , corpWalletJournal
       , corpWalletTransactions
       , eveAllianceList
       , eveCertificateTree
       , eveConquerableStationList
       , eveErrorList
       , eveFactionalWarfareStats
       , eveFactionalWarfareTop100
       , eveIDToName
       , eveNameToID
       , eveRefTypesList
       , eveSkillTree
       , mapFactionalWarfareOccupancyMap
       , mapJumps
       , mapKills
       , mapSovereignty
       , serverStatus
       )
-}
 where

import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Digest.Pure.SHA
import Data.List
import Data.Maybe
import Data.Time
import Network.HTTP
import Network.Stream
import Network.URI
import System.Locale
import Text.XML.Light

import EVE.LowLevel.DB

-----------------------------------------------------------------------------
-- Data types / classes for API keys
--

data LimitedAPIKey = LAPIK { limUserID  :: String, limAPIKey  :: String }
data FullAPIKey    = FAPIK { fullUserID :: String, fullAPIKey :: String }

class APIKey k where
  keyToArgs :: k -> [(String, String)]

instance APIKey LimitedAPIKey where
  keyToArgs (LAPIK x y) = [("userID", x), ("apiKey", y)]

instance APIKey FullAPIKey where
  keyToArgs (FAPIK x y) = [("userID", x), ("apiKey", y)]

-----------------------------------------------------------------------------
-- Errors and low-level types
--

data LowLevelError = ConnectionReset
                   | ConnectionClosed
                   | HTTPParseError String
                   | XMLParseError String
                   | EVEParseError Element
                   | UnknownError String
 deriving (Show)

type    LowLevelResult a = Either LowLevelError a
newtype CharacterID      = CID String
newtype ItemID           = IID String
newtype RefID            = RID String

{-
-----------------------------------------------------------------------------
-- The many, many API calls
--

-- http://wiki.eve-id.net/APIv2_Page_Index#Notes
-- is a very helpful page

characterList :: APIKey k => k -> IO LowLevelResult
characterList k = runRequest "account/Characters" (keyToArgs k)

charAccountBalances :: FullAPIKey -> CharacterID -> IO LowLevelResult
charAccountBalances = standardRequest "char/AccountBalance"

charAssetList :: FullAPIKey -> CharacterID -> IO LowLevelResult
charAssetList = extendedRequest [("version", "2")] "char/AssetList"

characterSheet :: APIKey k => k -> CharacterID -> IO LowLevelResult
characterSheet = standardRequest "char/CharacterSheet"

charFactionalWarfareStats :: APIKey k => k -> CharacterID -> IO LowLevelResult
charFactionalWarfareStats = standardRequest "char/FacWarStats"

charIndustryJobs :: FullAPIKey -> CharacterID -> IO LowLevelResult
charIndustryJobs = standardRequest "char/IndustryJobs"

charKillLogs :: FullAPIKey -> CharacterID -> Maybe RefID -> IO LowLevelResult
charKillLogs = walkableRequest "char/Killlog" "beforeKillID"

charMarketOrders :: FullAPIKey -> CharacterID -> IO LowLevelResult
charMarketOrders = standardRequest "char/MarketOrders"

charMedals :: APIKey k => k -> CharacterID -> IO LowLevelResult
charMedals = standardRequest "char/Medals"

charSkillInTraining :: APIKey k => k -> CharacterID -> IO LowLevelResult
charSkillInTraining = standardRequest "char/SkillInTraining"

charSkillQueue :: APIKey k => k -> CharacterID -> IO LowLevelResult
charSkillQueue = standardRequest "char/SkillQueue"

charStandings :: APIKey k => k -> CharacterID -> IO LowLevelResult
charStandings = standardRequest "char/Standings"

charWalletJournal :: FullAPIKey -> CharacterID -> Maybe RefID -> IO
                     LowLevelResult
charWalletJournal = walkableRequest "char/WalletJournal" "beforeRefID"

charWalletTransactions :: FullAPIKey -> CharacterID -> Maybe RefID ->
                          IO LowLevelResult
charWalletTransactions = 
  walkableRequest "char/WalletTransactions" "beforeTransID"

charMailMessages :: FullAPIKey -> CharacterID -> IO LowLevelResult
charMailMessages = standardRequest "char/MailMessages"

charNotifications :: FullAPIKey -> CharacterID -> IO LowLevelResult
charNotifications = standardRequest "char/Notifications"

charMailingLists :: FullAPIKey -> CharacterID -> IO LowLevelResult
charMailingLists = standardRequest "char/mailinglists"

corpAccountBalances :: FullAPIKey -> CharacterID -> IO LowLevelResult
corpAccountBalances = standardRequest "corp/AccountBalance"

corpAssetList :: FullAPIKey -> CharacterID -> IO LowLevelResult
corpAssetList = extendedRequest [("version", "2")] "corp/AssetList"

corpContainerLog :: FullAPIKey -> CharacterID -> IO LowLevelResult
corpContainerLog = standardRequest "corp/ContainerLog"

corporationSheet :: APIKey k => k -> CharacterID -> IO LowLevelResult
corporationSheet = standardRequest "corp/CorporationSheet"

corpFactionalWarfareStats :: APIKey k => k -> CharacterID -> IO LowLevelResult
corpFactionalWarfareStats = standardRequest "corp/FacWarStats"

corpIndustryJobs :: FullAPIKey -> CharacterID -> IO LowLevelResult
corpIndustryJobs = standardRequest "corp/IndustryJobs"

corpKillLogs :: FullAPIKey -> CharacterID -> Maybe RefID -> IO LowLevelResult
corpKillLogs = walkableRequest "corp/KillLog" "beforeKillID"

corpMarketOrders :: FullAPIKey -> CharacterID -> IO LowLevelResult
corpMarketOrders = standardRequest "corp/MarketOrders"

corpMedals :: APIKey k => k -> CharacterID -> IO LowLevelResult
corpMedals = standardRequest "corp/Medals"

corpMemberMedals :: APIKey k => k -> CharacterID -> IO LowLevelResult
corpMemberMedals = standardRequest "corp/MemberMedals"

corpMemberSecurity :: FullAPIKey -> CharacterID -> IO LowLevelResult
corpMemberSecurity = standardRequest "corp/MemberSecurity"

corpMemberSecurityLog :: FullAPIKey -> CharacterID -> IO LowLevelResult
corpMemberSecurityLog = standardRequest "corp/MemberSecurityLog"

corpMemberTracking :: FullAPIKey -> CharacterID -> IO LowLevelResult
corpMemberTracking = standardRequest "corp/MemberTracking"

corpPOSDetails :: FullAPIKey -> CharacterID -> ItemID -> IO LowLevelResult
corpPOSDetails k (CID cid) (IID iid) = runRequest "corp/StarbaseDetail" xs
 where
  xs      = [("version","2")] ++ keyToArgs k ++ details
  details = [("characterID", cid), ("itemID", iid)]

corpPOSList :: FullAPIKey -> CharacterID -> IO LowLevelResult
corpPOSList = extendedRequest [("version","2")] "corp/StarbaseList"

corpShareholders :: FullAPIKey -> CharacterID -> IO LowLevelResult
corpShareholders = standardRequest "corp/Shareholders"

corpStandings :: FullAPIKey -> CharacterID -> IO LowLevelResult
corpStandings = standardRequest "corp/Standings"

corpTitles :: FullAPIKey -> CharacterID -> IO LowLevelResult
corpTitles = standardRequest "corp/Titles"

corpWalletJournal :: FullAPIKey -> CharacterID -> Maybe RefID ->
                     IO LowLevelResult
corpWalletJournal = walkableRequest "corp/WalletJournal" "beforeRefID"

corpWalletTransactions :: FullAPIKey -> CharacterID -> Maybe RefID ->
                          IO LowLevelResult
corpWalletTransactions = 
  walkableRequest "corp/WalletTransactions" "beforeTransID"

eveAllianceList :: IO LowLevelResult
eveAllianceList = runRequest "eve/AllianceList" []

eveCertificateTree :: IO LowLevelResult
eveCertificateTree = runRequest "eve/CertificateTree" []

eveConquerableStationList :: IO LowLevelResult
eveConquerableStationList = runRequest "eve/ConquerableStationList" []

eveErrorList :: IO LowLevelResult
eveErrorList = runRequest "eve/ErrorList" []

eveFactionalWarfareStats :: IO LowLevelResult
eveFactionalWarfareStats = runRequest "eve/FacWarStats" []

eveFactionalWarfareTop100 :: IO LowLevelResult
eveFactionalWarfareTop100 = runRequest "eve/FacWarTopStats" []

eveIDToName :: [CharacterID] -> IO LowLevelResult
eveIDToName ids = runRequest "eve/CharacterName" [("Ids",ids')]
 where ids' = intercalate "," $ map (\ (CID x) -> x) ids

eveNameToID :: [String] -> IO LowLevelResult
eveNameToID names = runRequest "eve/CharacterID" [("names",names')]
 where names' = intercalate "," names

eveRefTypesList :: IO LowLevelResult
eveRefTypesList = runRequest "eve/RefTypes" []

eveSkillTree :: IO LowLevelResult
eveSkillTree = runRequest "eve/SkillTree" []

-}

mapFactionalWarfareOccupancyMap
  :: EVEDB -> IO (LowLevelResult [(Integer, String, Integer, String, Bool)])
mapFactionalWarfareOccupancyMap =
  runRequest "map/FacWarSystems" [] cachedUntil $ parseRows readRow
 where
  readRow r = do
    ssID   <- mread =<< findAttr (QName "solarSystemID"      Nothing Nothing) r
    ofID   <- mread =<< findAttr (QName "occupyingFactionID" Nothing Nothing) r
    cont   <- mread =<< findAttr (QName "contested"          Nothing Nothing) r
    ssName <- findAttr (QName "solarSystemName" Nothing Nothing) r
    ofName <- findAttr (QName "occupyingFactionName" Nothing Nothing) r
    return (ssID, ssName, ofID, ofName, cont)

-- |Returns a list (solarSystemID, numJumps). Note that there may not be an
-- entry in the returned list if numJumps = 0.
mapJumps :: EVEDB -> IO (LowLevelResult [(Integer, Integer)])
mapJumps = runRequest "map/Jumps" [] cachedUntil $ parseRows readRow
 where
  readRow row = do
    id     <- mread =<< findAttr (QName "solarSystemID"   Nothing Nothing) row
    jumps  <- mread =<< findAttr (QName "shipJumps"       Nothing Nothing) row
    return (id, jumps)

-- |Returns a list (solarSystemID, shipKills, factionKills, podKills)
mapKills :: EVEDB -> IO (LowLevelResult [(Integer, Integer, Integer, Integer)])
mapKills = runRequest "map/Kills" [] cachedUntil $ parseRows readRow
 where
  readRow :: Element -> Maybe (Integer, Integer, Integer, Integer)
  readRow row = do
    id     <- mread =<< findAttr (QName "solarSystemID"   Nothing Nothing) row
    shipKs <- mread =<< findAttr (QName "shipKills"       Nothing Nothing) row
    facKs  <- mread =<< findAttr (QName "factionKills"    Nothing Nothing) row
    podKs  <- mread =<< findAttr (QName "podKills"        Nothing Nothing) row
    return (id, shipKs, facKs, podKs)

data OwnerInfo = AllianceID Integer
               | CorporationID Integer
               | FactionID Integer
 deriving (Show, Eq)

data SolarSystem = SolarSystem {
       solarSystemID         :: Integer
     , solarSystemName       :: String
     , solarSystemOwner      :: [OwnerInfo]
     }
 deriving (Show, Eq)

mapSovereignty :: EVEDB -> IO (LowLevelResult [SolarSystem])
mapSovereignty = runRequest "map/Sovereignty" [] cachedUntil $ parseRows readRow
 where
  readRow :: Element -> Maybe SolarSystem
  readRow row = do
    id     <- mread =<< findAttr (QName "solarSystemID"   Nothing Nothing) row
    name   <-           findAttr (QName "solarSystemName" Nothing Nothing) row
    aliID  <- mread =<< findAttr (QName "allianceID"      Nothing Nothing) row
    corID  <- mread =<< findAttr (QName "corporationID"   Nothing Nothing) row
    facID  <- mread =<< findAttr (QName "factionID"       Nothing Nothing) row
    let list0 = []
        list1 = if aliID == 0 then list0 else (AllianceID aliID : list0)
        list2 = if corID == 0 then list1 else (CorporationID corID : list1)
        list3 = if facID == 0 then list2 else (FactionID facID : list2)
    return $ SolarSystem id name list3

serverStatus :: EVEDB -> IO (LowLevelResult (Bool, Integer))
serverStatus = runRequest "server/ServerStatus" [] cachedUntil pullResult
 where
  pullResult xml = fromMaybe (Left $ EVEParseError xml) $ do
    open    <- mread =<< getElementStringContent "serverOpen" xml
    players <- mread =<< getElementStringContent "onlinePlayers" xml
    return $ Right (open, players)

cachedUntil :: Element -> UTCTime
cachedUntil xml = fromMaybe zeroHour $ do
  str <- getElementStringContent "cachedUntil" xml
  parseTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" str

zeroHour :: UTCTime
zeroHour = UTCTime (toEnum 0) (toEnum 0)

mread :: Read a => String -> Maybe a
mread = fmap fst . listToMaybe . reads

parseRows :: (Element -> Maybe a) -> Element -> LowLevelResult [a]
parseRows f xml = maybe (Left $ EVEParseError xml) Right $ sequence $ map f rows
 where rows = findElements (QName "row" Nothing Nothing) xml

-----------------------------------------------------------------------------
-- Some helper functions to make writing the above less tedious.
--

walkableRequest :: APIKey k =>
                   String -> String -> 
                   (Element -> UTCTime) -> (Element -> LowLevelResult a) ->
                   EVEDB -> k -> CharacterID -> Maybe RefID ->
                   IO (LowLevelResult a)
walkableRequest proc name getExp finish db k c ref =
  extendedRequest extra proc getExp finish db k c
 where extra = case ref of
                 Nothing        -> []
                 Just (RID rid) -> [(name, rid)]

standardRequest :: APIKey k => 
                   String ->
                   (Element -> UTCTime) -> (Element -> LowLevelResult a) ->
                   EVEDB -> k -> CharacterID ->
                   IO (LowLevelResult a)
standardRequest = extendedRequest []

extendedRequest :: APIKey k =>
                   [(String, String)] -> String -> 
                   (Element -> UTCTime) -> (Element -> LowLevelResult a) ->
                   EVEDB -> k -> CharacterID ->
                   IO (LowLevelResult a)
extendedRequest extras proc getExp finish db key (CID cid) =
  runRequest proc args getExp finish db
 where args = keyToArgs key ++ extras ++ [("characterID", cid)]


runRequest :: String -> [(String, String)] ->
              (Element -> UTCTime) -> (Element -> LowLevelResult a) ->
              EVEDB -> IO (LowLevelResult a)
runRequest procedure args getExpireTime finishProcessing db =
  lookupCachedOrDo db reqHash parseResult runRequest
 where
  parseResult str =
    case parseXMLDoc str of
      Nothing -> Left  $ XMLParseError str
      Just r  -> finishProcessing r
  --
  runRequest = do
    res <- simpleHTTP req
    case res of
      Left ErrorReset     -> return $ Left   ConnectionReset
      Left ErrorClosed    -> return $ Left   ConnectionClosed
      Left (ErrorParse x) -> return $ Left $ HTTPParseError x
      Left (ErrorMisc  x) -> return $ Left $ UnknownError x
      Right resp          -> do
        let body = rspBody resp
        case parseXMLDoc body of
          Nothing         -> return $ Left $ XMLParseError body
          Just xml        -> do
            let expireTime = getExpireTime xml
                result     = finishProcessing xml
            addCachedResponse db reqHash body expireTime
            return result
  --
  Just uri = parseURI $ "http://api.eve-online.com/" ++ procedure ++ ".xml.aspx"
  req      = Request uri POST hdrs body
  hdrs     = [Header HdrContentType "application/x-www-form-urlencoded",
              Header HdrContentLength (show $ length body)]
  body     = intercalate "," $ map (\ (a,b) -> a ++ "=" ++ b) args
  reqHash  = showDigest $ sha512 $ BSC.pack $ show req ++ body

getElementStringContent :: String -> Element -> Maybe String
getElementStringContent name xml = do
  elem <- findElement (QName name Nothing Nothing) xml
  return $ strContent elem
