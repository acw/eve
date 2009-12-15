module EVE.LowLevel.API(
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
       , mapSovereigntyStatus
       , serverStatus
       )
 where

import Data.List
import Network.HTTP
import Network.Stream
import Network.URI
import Text.XML.Light

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
                   | UnknownError String
 deriving (Show,Eq)

type    LowLevelResult = Either LowLevelError Element
newtype CharacterID    = CID String

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

charKillLogs :: FullAPIKey -> IO LowLevelResult
charKillLogs = undefined

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

charWalletJournal :: FullAPIKey -> IO LowLevelResult
charWalletJournal= undefined

charWalletTransactions :: FullAPIKey -> IO LowLevelResult
charWalletTransactions= undefined

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

corpKillLogs :: FullAPIKey -> IO LowLevelResult
corpKillLogs = undefined

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

corpPOSDetails :: FullAPIKey -> IO LowLevelResult
corpPOSDetails = undefined

corpPOSList :: FullAPIKey -> CharacterID -> IO LowLevelResult
corpPOSList = extendedRequest [("version","2")] "corp/StarbaseList"

corpShareholders :: FullAPIKey -> CharacterID -> IO LowLevelResult
corpShareholders = standardRequest "corp/Shareholders"

corpStandings :: FullAPIKey -> CharacterID -> IO LowLevelResult
corpStandings = standardRequest "corp/Standings"

corpTitles :: FullAPIKey -> CharacterID -> IO LowLevelResult
corpTitles = standardRequest "corp/Titles"

corpWalletJournal :: FullAPIKey -> IO LowLevelResult
corpWalletJournal= undefined

corpWalletTransactions :: FullAPIKey -> IO LowLevelResult
corpWalletTransactions= undefined

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

mapFactionalWarfareOccupancyMap :: IO LowLevelResult
mapFactionalWarfareOccupancyMap = runRequest "map/FacWarSystems" []

mapJumps :: IO LowLevelResult
mapJumps = runRequest "map/Jumps" []

mapKills :: IO LowLevelResult
mapKills = runRequest "map/Kills" []

mapSovereignty :: IO LowLevelResult
mapSovereignty = runRequest "map/Sovereignty" []

mapSovereigntyStatus :: IO LowLevelResult
mapSovereigntyStatus = runRequest "map/SovereigntyStatus" []

serverStatus :: IO LowLevelResult
serverStatus = runRequest "server/ServerStatus" []

-----------------------------------------------------------------------------
-- Some helper functions to make writing the above less tedious.
--

extendedRequest :: APIKey k =>
                   [(String, String)] -> String -> k -> CharacterID -> 
                   IO LowLevelResult
extendedRequest extras proc key (CID cid) = runRequest proc args
 where args = keyToArgs key ++ extras ++ [("characterID", cid)]

standardRequest :: APIKey k => String -> k -> CharacterID -> IO LowLevelResult
standardRequest = extendedRequest []

runRequest :: String -> [(String, String)] -> IO LowLevelResult
runRequest procedure args = do
  res <- simpleHTTP req
  return $ case res of
             Left ErrorReset     -> Left   ConnectionReset
             Left ErrorClosed    -> Left   ConnectionClosed
             Left (ErrorParse x) -> Left $ HTTPParseError x
             Left (ErrorMisc x)  -> Left $ UnknownError x
             Right response      ->
               case parseXMLDoc $ rspBody response of
                 Nothing         -> Left $ XMLParseError $ rspBody response
                 Just xml        -> Right xml
 where
  Just uri = parseURI $ "http://api.eve-online.com/" ++ procedure ++ ".xml.aspx"
  req      = Request uri POST hdrs body
  hdrs     = [Header HdrContentType "application/x-www-form-urlencoded",
              Header HdrContentLength (show $ length body)]
  body     = intercalate "," $ map (\ (a,b) -> a ++ "=" ++ b) args
