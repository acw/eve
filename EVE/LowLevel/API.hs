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

import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Digest.Pure.SHA
import Data.List
import Data.Maybe
import qualified Data.Map as Map
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
corpPOSDetails k (CharID cid) (IID iid) = runRequest "corp/StarbaseDetail" xs
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

-}

eveAllianceList :: EVEDB -> IO (LowLevelResult [Alliance])
eveAllianceList = runRequest "eve/AllianceList" [] parse
 where
  parse xml = maybe (Left $ EVEParseError xml) Right $ do
    rows <- findElementWithAttName "alliances" xml
    mapChildren "row" rows $ \ r -> do
      name  <-                           findAttr (unqual "name")           r
      sname <-                           findAttr (unqual "shortName")      r
      allID <- AllianceID <$> (mread =<< findAttr (unqual "allianceID")     r)
      exeID <- CorpID     <$> (mread =<< findAttr (unqual "executorCorpID") r)
      cnt   <-                 mread =<< findAttr (unqual "memberCount")    r
      start <-                 tread =<< findAttr (unqual "startDate")      r
      mset  <- findChildWithAttName "memberCorporations" r
      mems  <- mapChildren "row" mset $ \ mem -> do
                 c <- mread =<< findAttr (unqual "corporationID") mem
                 d <- tread =<< findAttr (unqual "startDate")     mem
                 return (CorpID c, d)
      return (Alliance allID name sname exeID cnt start mems)

eveCertificateTree :: EVEDB -> IO (LowLevelResult ([CertificateCategory],
                                                   [CertificateClass],
                                                   [Certificate]))
eveCertificateTree = runRequest "eve/CertificateTree" [] parse
 where
  parse xml = maybe (Left $ EVEParseError xml) Right $ do
    rows <- findElementWithAttName "categories" xml
    foldChildren "row" rows ([],[],[]) $ \ (cats, classes, certs) c -> do
      catID <- CCatID <$> (mread =<< findAttr (unqual "categoryID")   c)
      catNm <-                       findAttr (unqual "categoryName") c
      cset  <-                       findChildWithAttName "classes"   c
      let newAcc = (CCat catID catNm : cats, classes, certs)
      foldChildren "row" cset newAcc $ \ (cats2, classes2, certs2) l -> do
        clID   <- mread =<< findAttr (unqual "classID")   l
        clName <-           findAttr (unqual "className") l
        ceset  <- findChildWithAttName "certificates" l
        let newAcc' = (cats, CClass (CClassID clID) clName : classes2, certs2)
        foldChildren "row" ceset newAcc' $ \ (cats3, classes3, certs3) x -> do
          ceID    <- mread =<< findAttr (unqual "certificateID") x
          ceGrade <- mread =<< findAttr (unqual "grade")         x
          ceCorp  <- mread =<< findAttr (unqual "corporationID") x
          ceDesc  <-           findAttr (unqual "description")   x
          sreqs   <- case findChildWithAttName "requiredSkills" x of
                       Nothing   -> Just []
                       Just sset -> fmap (map SkillReq) $
                         mapChildren "row" sset $ \ r -> do
                           sid <- mread =<< findAttr (unqual "typeID") r
                           slv <- mread =<< findAttr (unqual "level")  r
                           return $ SkillLevel (SkillID sid) slv
          creqs   <- case findChildWithAttName "requiredCertificates" x of
                       Nothing   -> Just []
                       Just rset -> fmap (map CertificateReq) $
                         mapChildren "row" rset $ \ r -> do
                           cid <- mread =<< findAttr (unqual "certificateID") r
                           clv <- mread =<< findAttr (unqual "grade")         r
                           return $ CertLevel (CertID cid) clv
          let newCert = Certificate (CertID ceID) catID (CClassID clID)
                                    (CorpID ceCorp) ceDesc ceGrade
                                    (sreqs ++ creqs)
          return (cats3, classes3, newCert : certs3)

eveConquerableStationList :: EVEDB -> IO (LowLevelResult [ConquerableStation])
eveConquerableStationList =
  runRequest "eve/ConquerableStationList" [] $ parseRows $ \ r -> do
    sID <- StatID <$> (mread =<< findAttr (unqual "stationID")       r)
    sNm <-                       findAttr (unqual "stationName")     r
    sTp <-             mread =<< findAttr (unqual "stationTypeID")   r
    sSS <- SSID   <$> (mread =<< findAttr (unqual "solarSystemID")   r)
    cID <- CorpID <$> (mread =<< findAttr (unqual "corporationID")   r)
    cNm <-                       findAttr (unqual "corporationName") r
    return (CStat sID sNm sTp sSS cID cNm)

eveErrorList :: EVEDB -> IO (LowLevelResult [(Integer,String)])
eveErrorList = runRequest "eve/ErrorList" [] $ parseRows $ \ r -> do
  code <- mread =<< findAttr (unqual "errorCode") r
  str  <-           findAttr (unqual "errorText") r
  return (code, str)

eveFactionalWarfareStats :: EVEDB ->
                            IO (LowLevelResult (KillTotals, [FactionStats]))
eveFactionalWarfareStats = runRequest "eve/FacWarStats" [] parse
 where
  parse xml = maybe (Left $ EVEParseError xml) Right $ do
    kyest <-  mread =<< getElementData "killsYesterday"         xml
    kweek <-  mread =<< getElementData "killsLastWeek"          xml
    ktot  <-  mread =<< getElementData "killsTotal"             xml
    vyest <-  mread =<< getElementData "victoryPointsYesterday" xml
    vweek <-  mread =<< getElementData "victoryPointsLastWeek"  xml
    vtot  <-  mread =<< getElementData "victoryPointsTotal"     xml
    facs  <-    findElementWithAttName "factions"               xml
    facws <-    findElementWithAttName "factionWars"            xml
    let totals = KillTotals kyest kweek ktot vyest vweek vtot
    wmap  <- foldChildren "row" facws Map.empty $ \ acc row -> do
               facid <- mread =<< findAttr (unqual "factionID")   row
               othid <- mread =<< findAttr (unqual "againstID")   row
               othn  <-           findAttr (unqual "againstName") row
               return $ Map.insertWith (++) facid [(othid, othn)] acc
    stats <- foldChildren "row" facs []         $ \ acc row -> do
               facID <- mread =<< findAttr (unqual "factionID")              row
               facn  <-           findAttr (unqual "factionName")            row
               pils  <- mread =<< findAttr (unqual "pilots")                 row
               sc    <- mread =<< findAttr (unqual "systemsControlled")      row
               ky    <- mread =<< findAttr (unqual "killsYesterday")         row
               kl    <- mread =<< findAttr (unqual "killsLastWeek")          row
               kt    <- mread =<< findAttr (unqual "killsTotal")             row
               vy    <- mread =<< findAttr (unqual "victoryPointsYesterday") row
               vl    <- mread =<< findAttr (unqual "victoryPointsLastWeek")  row
               vt    <- mread =<< findAttr (unqual "victoryPointsTotal")     row
               enms  <- Map.lookup facID wmap
               let kills = KillTotals ky kl kt vy vl vt
               return $ FactionStats facID facn pils sc kills enms : acc
    return (totals, stats)

eveFactionalWarfareTop100 :: EVEDB -> IO (LowLevelResult KillStats)
eveFactionalWarfareTop100 = runRequest "eve/FacWarTopStats" [] parse
 where
  parse xml = maybe (Left $ EVEParseError xml) Right $ do
    chs       <- findElement (unqual "characters")   xml
    cos       <- findElement (unqual "corporations") xml
    fas       <- findElement (unqual "factions")     xml
    charStats <- processStats "characterID"   "characterName"   CharID chs
    corpStats <- processStats "corporationID" "corporationName" CorpID cos
    facStats  <- processStats "factionID"     "factionName"     FacID  fas
    return $ KillStats charStats corpStats facStats
  processStats i n b xml = do
    kyest  <- findChildWithAttName "KillsYesterday"         xml
    kweek  <- findChildWithAttName "KillsLastWeek"          xml
    ktot   <- findChildWithAttName "KillsTotal"             xml
    vyest  <- findChildWithAttName "VictoryPointsYesterday" xml
    vweek  <- findChildWithAttName "VictoryPointsLastWeek"  xml
    vtot   <- findChildWithAttName "VictoryPointsTotal"     xml
    kyest' <- toList i n "kills"         b kyest
    kweek' <- toList i n "kills"         b kweek
    ktot'  <- toList i n "kills"         b ktot
    vyest' <- toList i n "victoryPoints" b vyest
    vweek' <- toList i n "victoryPoints" b vweek
    vtot'  <- toList i n "victoryPoints" b vtot
    return $ KillList kyest' kweek' ktot' vyest' vweek' vtot'
  toList :: String -> String -> String -> (Integer -> a) -> Element -> Maybe [(a, String, Integer)]
  toList idAtt nameAtt valAtt builder xml = mapChildren "row" xml $ \ row -> do
    id    <- builder <$> (mread =<< findAttr (unqual idAtt)   row)
    name  <-                        findAttr (unqual nameAtt) row
    val   <-              mread =<< findAttr (unqual valAtt)  row
    return (id, name, val)

eveIDToName :: [CharacterID] -> EVEDB ->
               IO (LowLevelResult [(String,CharacterID)])
eveIDToName ids =
  runRequest "eve/CharacterName" [("Ids",ids')] readNameIDs
 where ids' = intercalate "," $ map (\ (CharID x) -> show x) ids

eveNameToID :: [String] -> EVEDB -> 
               IO (LowLevelResult [(String,CharacterID)])
eveNameToID names =
  runRequest "eve/CharacterID" [("names",names')] readNameIDs
 where names'    = intercalate "," names

readNameIDs :: Element -> LowLevelResult [(String,CharacterID)]
readNameIDs = parseRows $ \ r -> do
  name <-           findAttr (unqual "name")        r
  cid  <- mread =<< findAttr (unqual "characterID") r
  return (name, CharID cid)

eveRefTypesList :: EVEDB -> IO (LowLevelResult [(Integer,String)])
eveRefTypesList = runRequest "eve/RefTypes" [] $ parseRows readRow
 where
  readRow r = do
    refid   <- mread =<< findAttr (unqual "refTypeID")   r
    refname <-           findAttr (unqual "refTypeName") r
    return (refid, refname)

eveSkillTree :: EVEDB -> IO (LowLevelResult ([SkillGroup], [Skill]))
eveSkillTree = runRequest "eve/SkillTree" [] parseResults
 where
  parseResults :: Element -> LowLevelResult ([SkillGroup], [Skill])
  parseResults xml = maybe (Left $ EVEParseError xml) Right $ do
    rows <- findElementWithAttName "skillGroups" xml
    foldChildren "row" rows ([],[]) $ \ (groups,skills) grow -> do
      name <- findAttr (unqual "groupName") grow
      gid  <- mread =<< findAttr (unqual "groupID") grow
      let newGroup = SkillGroup name (SkillGroupID gid)
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
                     return (SkillLevel (SkillID reqtid) reqsl)
          rbons <- findChildWithAttName "skillBonusCollection" srow
          bons  <- mapChildren "row" rbons $ \ bon -> do
                     btype <- findAttr (unqual "bonusType") bon
                     bval  <- findAttr (unqual "bonusValue") bon
                     return (btype, bval)
          let (reqs',bons') = parseBonuses bons
              skill         = Skill {
                                skillName         = sname
                              , skillGroup        = SkillGroupID group
                              , skillID           = SkillID tid
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
  runRequest "map/FacWarSystems" [] $ parseRows readRow
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
mapJumps = runRequest "map/Jumps" [] $ parseRows readRow
 where
  readRow row = do
    id     <- mread =<< findAttr (unqual "solarSystemID") row
    jumps  <- mread =<< findAttr (unqual "shipJumps") row
    return (id, jumps)

-- |Returns a list (solarSystemID, shipKills, factionKills, podKills)
mapKills :: EVEDB -> IO (LowLevelResult [(Integer, Integer, Integer, Integer)])
mapKills = runRequest "map/Kills" [] $ parseRows readRow
 where
  readRow :: Element -> Maybe (Integer, Integer, Integer, Integer)
  readRow row = do
    id     <- mread =<< findAttr (unqual "solarSystemID") row
    shipKs <- mread =<< findAttr (unqual "shipKills")     row
    facKs  <- mread =<< findAttr (unqual "factionKills")  row
    podKs  <- mread =<< findAttr (unqual "podKills")      row
    return (id, shipKs, facKs, podKs)

mapSovereignty :: EVEDB -> IO (LowLevelResult [SolarSystem])
mapSovereignty = runRequest "map/Sovereignty" [] $ parseRows readRow
 where
  readRow :: Element -> Maybe SolarSystem
  readRow row = do
    id     <- SSID       <$> (mread =<< findAttr (unqual "solarSystemID")   row)
    name   <-           findAttr (unqual "solarSystemName") row
    aliID  <- AllianceID <$> (mread =<< findAttr (unqual "allianceID")      row)
    corID  <- CorpID     <$> (mread =<< findAttr (unqual "corporationID")   row)
    facID  <- FacID      <$> (mread =<< findAttr (unqual "factionID")       row)
    let list0 = []
        list1 = if aliID == noAll  then list0 else (OwnerAlliance aliID : list0)
        list2 = if corID == noCorp then list1 else (OwnerCorp     corID : list1)
        list3 = if facID == noFac  then list2 else (OwnerFaction  facID : list2)
    return $ SolarSystem id name list3

serverStatus :: EVEDB -> IO (LowLevelResult (Bool, Integer))
serverStatus = runRequest "server/ServerStatus" [] pullResult
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

tread :: ParseTime t => String -> Maybe t
tread = fmap fst . listToMaybe . readsTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

parseRows :: (Element -> Maybe a) -> Element -> LowLevelResult [a]
parseRows f xml = maybe (Left $ EVEParseError xml) Right $ sequence $ map f rows
 where rows = findElements (unqual "row") xml

-----------------------------------------------------------------------------
-- Some helper functions to make writing the above less tedious.
--

walkableRequest :: APIKey k =>
                   String -> String ->
                   (Element -> LowLevelResult a) ->
                   EVEDB -> k -> CharacterID -> Maybe RefID ->
                   IO (LowLevelResult a)
walkableRequest proc name finish db k c ref =
  extendedRequest extra proc finish db k c
 where extra = case ref of
                 Nothing        -> []
                 Just (RID rid) -> [(name, rid)]

standardRequest :: APIKey k => 
                   String -> (Element -> LowLevelResult a) ->
                   EVEDB -> k -> CharacterID ->
                   IO (LowLevelResult a)
standardRequest = extendedRequest []

extendedRequest :: APIKey k =>
                   [(String, String)] -> String ->
                   (Element -> LowLevelResult a) ->
                   EVEDB -> k -> CharacterID ->
                   IO (LowLevelResult a)
extendedRequest extras proc finish db key (CharID cid) =
  runRequest proc args finish db
 where args = keyToArgs key ++ extras ++ [("characterID", show cid)]


runRequest :: String -> [(String, String)] ->
              (Element -> LowLevelResult a) ->
              EVEDB -> IO (LowLevelResult a)
runRequest procedure args finishProcessing db =
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
            let expireTime = cachedUntil xml
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
