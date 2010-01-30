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
import Text.XML.Light.Helpers

import EVE.LowLevel.DB
import EVE.LowLevel.Types

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

-}

eveSkillTree :: EVEDB -> IO (LowLevelResult ([SkillGroup], [Skill]))
eveSkillTree = runRequest "eve/SkillTree" [] cachedUntil parseResults
 where
  parseResults :: Element -> LowLevelResult ([SkillGroup], [Skill])
  parseResults xml = maybe (Left $ EVEParseError xml) Right $ do
    rows <- findElementWithAttName "skillGroups" xml
    foldChildren "row" rows ([],[]) $ \ (groups,skills) grow -> do
      name <- findAttr (unqual "groupName") grow
      gid  <- mread =<< findAttr (unqual "groupID") grow
      let newGroup = SkillGroup name gid
      foldChildren "rowset" grow (newGroup:groups, skills) $ \ acc srows ->
        foldChildren "row" srows acc $ \ (groups', skills') srow -> do
          sname <-           findAttr (unqual "typeName") srow
          tid   <- mread =<< findAttr (unqual "typeID") srow
          group <- mread =<< findAttr (unqual "groupID") srow
          desc  <-           getChildData "description" srow
          rank  <- mread =<< getChildData "rank" srow
          pri   <- mread =<< getElementData "primaryAttribute" srow
          sec   <- mread =<< getElementData "secondaryAttribute" srow
          unless (group == gid) $ fail "group is not equal to id!"
          rreqs <- findChildWithAttName "requiredSkills" srow
          reqs  <- mapChildren "row" rreqs $ \ req -> do
                     reqtid <- mread =<< findAttr (unqual "typeID") req
                     reqsl  <- mread =<< findAttr (unqual "skillLevel") req
                     return (SkillLevel reqtid reqsl)
          rbons <- findChildWithAttName "skillBonusCollection" srow
          bons  <- mapChildren "row" rbons $ \ bon -> do
                     btype <- findAttr (unqual "bonusType") bon
                     bval  <- findAttr (unqual "bonusValue") bon
                     return (btype, bval)
          let (reqs',bons') = parseBonuses bons
              skill         = Skill {
                                skillName         = sname
                              , skillGroup        = group
                              , skillID           = tid
                              , skillDescription  = desc
                              , skillRank         = rank
                              , skillPrimary      = pri
                              , skillSecondary    = sec
                              , skillRequirements = reqs ++ reqs'
                              , skillBonuses      = bons'
                              }
          return (groups', skill : skills')

mapFactionalWarfareOccupancyMap
  :: EVEDB -> IO (LowLevelResult [(Integer, String, Integer, String, Bool)])
mapFactionalWarfareOccupancyMap =
  runRequest "map/FacWarSystems" [] cachedUntil $ parseRows readRow
 where
  readRow r = do
    ssID   <- mread =<< findAttr (unqual "solarSystemID")      r
    ofID   <- mread =<< findAttr (unqual "occupyingFactionID") r
    cont   <- mread =<< findAttr (unqual "contested")          r
    ssName <-           findAttr (unqual "solarSystemName") r
    ofName <-           findAttr (unqual "occupyingFactionName") r
    return (ssID, ssName, ofID, ofName, cont)

-- |Returns a list (solarSystemID, numJumps). Note that there may not be an
-- entry in the returned list if numJumps = 0.
mapJumps :: EVEDB -> IO (LowLevelResult [(Integer, Integer)])
mapJumps = runRequest "map/Jumps" [] cachedUntil $ parseRows readRow
 where
  readRow row = do
    id     <- mread =<< findAttr (unqual "solarSystemID") row
    jumps  <- mread =<< findAttr (unqual "shipJumps") row
    return (id, jumps)

-- |Returns a list (solarSystemID, shipKills, factionKills, podKills)
mapKills :: EVEDB -> IO (LowLevelResult [(Integer, Integer, Integer, Integer)])
mapKills = runRequest "map/Kills" [] cachedUntil $ parseRows readRow
 where
  readRow :: Element -> Maybe (Integer, Integer, Integer, Integer)
  readRow row = do
    id     <- mread =<< findAttr (unqual "solarSystemID") row
    shipKs <- mread =<< findAttr (unqual "shipKills")     row
    facKs  <- mread =<< findAttr (unqual "factionKills")  row
    podKs  <- mread =<< findAttr (unqual "podKills")      row
    return (id, shipKs, facKs, podKs)

mapSovereignty :: EVEDB -> IO (LowLevelResult [SolarSystem])
mapSovereignty = runRequest "map/Sovereignty" [] cachedUntil $ parseRows readRow
 where
  readRow :: Element -> Maybe SolarSystem
  readRow row = do
    id     <- mread =<< findAttr (unqual "solarSystemID")   row
    name   <-           findAttr (unqual "solarSystemName") row
    aliID  <- mread =<< findAttr (unqual "allianceID")      row
    corID  <- mread =<< findAttr (unqual "corporationID")   row
    facID  <- mread =<< findAttr (unqual "factionID")       row
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
 where rows = findElements (unqual "row") xml

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
  elem <- findElement (unqual name) xml
  return $ strContent elem
