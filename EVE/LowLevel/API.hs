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
       , charMailingLists
       , charMailMessages
       , charMarketOrders
       , charMedals
       , charSkillInTraining
       , charSkillQueue
       , charStandings
       , charWalletJournal
       , charWalletTransactions
       , charNotifications
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

-----------------------------------------------------------------------------
-- The many, many API calls
--

-- http://wiki.eve-id.net/APIv2_Page_Index#Notes
-- is a very helpful page

characterList :: APIKey k => k -> EVEDB ->
                 IO (LowLevelResult [(String,CharacterID,String,CorporationID)])
characterList k =
 runRequest "account/Characters" (keyToArgs k) $ parseRows $ \ r -> do
  name <-                       findAttr (unqual "name")            r
  char <- CharID <$> (mread =<< findAttr (unqual "characterID")     r)
  cnam <-                       findAttr (unqual "corporationName") r
  corp <- CorpID <$> (mread =<< findAttr (unqual "corporationID")   r)
  return (name, char, cnam, corp)

charAccountBalances :: EVEDB -> FullAPIKey -> CharacterID ->
                       IO (LowLevelResult [(AccountID, Double)])
charAccountBalances =
 standardRequest "char/AccountBalance" $ parseRows $ \ r -> do
  accountID <- AccID <$> (mread =<< findAttr (unqual "accountID") r)
  amount    <-            mread =<< findAttr (unqual "balance")   r
  return (accountID, amount)

charAssetList :: EVEDB -> FullAPIKey -> CharacterID ->
                 IO (LowLevelResult [Item])
charAssetList =
 extendedRequest [("version", "2")] "char/AssetList" $ \ x ->
  maybe (Left $ EVEParseError x) Right $ do
   rs0 <- findElement (unqual "result") x
   rs1 <- findChild   (unqual "rowset") rs0
   foldChildren "row" rs1 [] $ \ acc cur -> do
     res <- parseItem Nothing cur
     return (res : acc)

parseItem :: Maybe LocationID -> Element -> Maybe Item
parseItem mlid el = do
  iid <- IID       <$> (mread =<< findAttr (unqual "itemID")     el)
  tid <- TID       <$> (mread =<< findAttr (unqual "typeID")     el)
  qnt <-                mread =<< findAttr (unqual "quantity")   el
  flg <- toLocFlag =<< (mread =<< findAttr (unqual "flag")       el)
  sng <- (== 0)    <$> (mread =<< findAttr (unqual "singleton")  el)
  lid <- case mlid of
           Nothing -> LocID <$> (mread =<< findAttr (unqual "locationID") el)
           Just x  -> return x
  sub <- case findChild (unqual "rowset") el of
           Just rs ->
             foldChildren "row" rs [] $ \ acc cur -> do
               res <- parseItem (Just lid) cur
               return (res : acc)
           Nothing ->
             return []
  return (Item iid (lid, flg) tid qnt sng sub)

characterSheet :: APIKey k =>
                  EVEDB -> k -> CharacterID ->
                  IO (LowLevelResult Character)
characterSheet = standardRequest "char/CharacterSheet" $ \ x ->
 maybe (Left $ EVEParseError x) Right $ do
  cid <- CharID <$> (mread =<< getElementData "characterID"     x)
  nm  <-                       getElementData "name"            x
  rc  <-                       getElementData "race"            x
  bl  <-                       getElementData "bloodLine"       x
  gn  <-             mread =<< getElementData "gender"          x
  cn  <-                       getElementData "corporationName" x
  ci  <- CorpID <$> (mread =<< getElementData "corporationID"   x)
  bal <-             mread =<< getElementData "balance"         x
  int <-             mread =<< getElementData "intelligence"    x
  mem <-             mread =<< getElementData "memory"          x
  cha <-             mread =<< getElementData "charisma"        x
  per <-             mread =<< getElementData "perception"      x
  wil <-             mread =<< getElementData "willpower"       x
  let enhs = catMaybes [getEnhancer "intelligenceBonus" Intelligence x
                       ,getEnhancer "memoryBonus"       Memory       x
                       ,getEnhancer "charismaBonus"     Charisma     x
                       ,getEnhancer "perceptionBonus"   Perception   x
                       ,getEnhancer "willpowerBonus"    Willpower    x]
  sks <- do base <- findElementWithAttName "skills" x
            mapChildren "row" base $ \ sk -> do
              typ <- SkillID <$> (mread =<< findAttr (unqual "typeID")      sk)
              lev <-              mread =<< findAttr (unqual "level")       sk
              pnt <-              mread =<< findAttr (unqual "skillpoints") sk
              return (SkillLevel typ lev, pnt)
  return (Character cid nm rc bl gn cn ci bal enhs int mem cha per wil sks)
 where
  getEnhancer :: String -> Attribute -> Element -> Maybe AttributeEnhancer
  getEnhancer s attr xml = do
    el <- findElement (unqual s) xml
    nm <-           getChildData "augmentatorName" el
    vl <- mread =<< getChildData "augmentatorValue" el
    return (AttrEnh attr nm vl)

charFactionalWarfareStats :: APIKey k => 
                             EVEDB -> k -> CharacterID ->
                             IO (LowLevelResult (Maybe CharWarfareStats))
charFactionalWarfareStats = standardRequest "char/FacWarStats" $ \ x ->
  maybe (Left $ EVEParseError x) Right $ actualData x <|> notInvolved x
 where
  actualData x = do
    fid <- FacID <$> (mread =<< getElementData "factionID"   x)
    fnm <-            mread =<< getElementData "factionName" x
    enl <-            tread =<< getElementData "enlisted"    x
    crn <-            mread =<< getElementData "currentRank" x
    hrn <-            mread =<< getElementData "highestRank" x
    kst <- readKillStats x
    return (Just $ CharWarfareStats fid fnm enl crn hrn kst)
  notInvolved x = do
    err  <- findElement (unqual "error") x
    code <- mread =<< findAttr (unqual "code") err
    guard (code == 124)
    return Nothing

{-
 - Skip this one for a bit
 -
charIndustryJobs :: FullAPIKey -> CharacterID -> 
                    IO (LowLevelResult [IndustryJob])
charIndustryJobs = standardRequest "char/IndustryJobs" $ parseRows $ \ r -> do
-}

charKillLogs :: EVEDB -> FullAPIKey -> CharacterID -> Maybe RefID ->
                IO (LowLevelResult [Kill])
charKillLogs = walkableRequest "char/Killlog" "beforeKillID" $ \ xml ->
 maybe (Left $ EVEParseError xml) Right $ readKillLog xml

readKillLog :: Element -> Maybe [Kill]
readKillLog xml = do
  set <- findElementWithAttName "kills" xml
  mapChildren "row" set $ \ kr -> do
    vic  <- findChild            (unqual "victim")   kr
    atts <- findChildWithAttName         "attackers" kr
    itms <- findChildWithAttName         "items"     kr
    kid  <- RID        <$> (mread =<< findAttr (unqual "killID")          kr)
    ktm  <-                 tread =<< findAttr (unqual "killTime")        kr
    kloc <- SSID       <$> (mread =<< findAttr (unqual "solarSystemID")   kr)
    --
    vid  <- CharID     <$> (mread =<< findAttr (unqual "characterID")     vic)
    vnm  <-                           findAttr (unqual "characterName")   vic
    vcoi <- CorpID     <$> (mread =<< findAttr (unqual "corporationID")   vic)
    vcon <-                           findAttr (unqual "corporationName") vic
    vall <- AllianceID <$> (mread =<< findAttr (unqual "allianceID")      vic)
    vpnt <-                 mread =<< findAttr (unqual "damageTaken")     vic
    vshp <- TID        <$> (mread =<< findAttr (unqual "shipTypeID")      vic)
    att  <- mapChildren "row" atts $ \ ar -> do
      aid  <- CharID     <$> (mread =<< findAttr (unqual "characterID")     ar)
      anm  <-                           findAttr (unqual "characterName")   ar
      acoi <- CorpID     <$> (mread =<< findAttr (unqual "corporationID")   ar)
      acon <-                           findAttr (unqual "corporationName") ar
      aall <- AllianceID <$> (mread =<< findAttr (unqual "allianceID")      ar)
      aaln <-                           findAttr (unqual "allianceName")    ar
      asec <-                 mread =<< findAttr (unqual "securityStatus")  ar
      admg <-                 mread =<< findAttr (unqual "damageDone")      ar
      afnl <- (== 1)     <$> (mread =<< findAttr (unqual "finalBlow")       ar)
      awep <- TID        <$> (mread =<< findAttr (unqual "weaponTypeID")    ar)
      ashp <- TID        <$> (mread =<< findAttr (unqual "shipTypeID")      ar)
      let base = (aid, anm, acoi, acon, aall, aaln)
      return $ AttackerInfo base asec admg afnl awep ashp
    itm <- mapChildren "row" itms $ \ ir -> do
      ityp <- TID        <$> (mread =<< findAttr (unqual "typeID")       ir)
      iflg <- toLocFlag  =<< (mread =<< findAttr (unqual "flag")         ir)
      idrp <-                 mread =<< findAttr (unqual "qtyDropped")   ir
      idst <-                 mread =<< findAttr (unqual "qtyDestroyed") ir
      return (ityp, iflg, idrp, idst)
    let victim = (vid, vnm, vcoi, vcon, vall)
    return $ Kill kid ktm kloc victim vpnt vshp att itm

charMailingLists :: EVEDB -> FullAPIKey -> CharacterID ->
                    IO (LowLevelResult [(ListID, String)])
charMailingLists = standardRequest "char/mailinglists" $ parseRows $ \ r -> do
  lid <- ListID <$> (mread =<< findAttr (unqual "listID")      r)
  nam <-                       findAttr (unqual "displayName") r
  return (lid, nam)

charMailMessages :: EVEDB -> FullAPIKey -> CharacterID ->
                    IO (LowLevelResult [MailMessage])
charMailMessages = standardRequest "char/MailMessages" $ parseRows $ \ r -> do
  mid <- MsgID  <$> (mread =<< findAttr (unqual "messageID")          r)
  sid <- CharID <$> (mread =<< findAttr (unqual "senderID")           r)
  dat <-             tread =<< findAttr (unqual "sentDate")           r
  tit <-                       findAttr (unqual "title")              r
  tco <-                       findAttr (unqual "toCorpOrAllianceID") r
  tli <-                       findAttr (unqual "toListIDs")          r
  tch <-                       findAttr (unqual "toCharacterIDs")     r
  rd  <- (== 1) <$> (mread =<< findAttr (unqual "read")               r)
  dst <- liftM3 (\ a b c -> a ++ b ++ c)
                (unwindCommas (ToCorpOrAlliance . CorpID) tco)
                (unwindCommas (ToCharacter      . CharID) tch)
                (unwindCommas (ToMailingList    . ListID) tli)
  return (MailMessage mid sid dst dat tit rd)
 where
  unwindCommas f "" = return []
  unwindCommas f xs = do let (start,rest) = break (/= ',') xs
                         rest' <- unwindCommas f (drop 1 xs)
                         cur   <- f <$> mread start
                         return (cur:rest')

charMarketOrders :: EVEDB -> FullAPIKey -> CharacterID ->
                    IO (LowLevelResult [MarketOrder])
charMarketOrders = standardRequest "char/MarketOrders" $ parseRows readOrder

readOrder :: Element -> Maybe MarketOrder
readOrder r = do
  oid <- OrderID      <$> (mread =<< findAttr (unqual "orderID")      r)
  cid <- CharID       <$> (mread =<< findAttr (unqual "charID")       r)
  sid <- StatID       <$> (mread =<< findAttr (unqual "stationID")    r)
  ovl <-                   mread =<< findAttr (unqual "volEntered")   r
  nvl <-                   mread =<< findAttr (unqual "volRemaining") r
  mvl <-                   mread =<< findAttr (unqual "minVolume")    r
  ost <- toOrderState <$> (mread =<< findAttr (unqual "orderState")   r)
  tid <- TID          <$> (mread =<< findAttr (unqual "typeID")       r)
  rng <- toRange      <$> (mread =<< findAttr (unqual "range")        r)
  aky <-                   mread =<< findAttr (unqual "accountKey")   r
  dur <-                   mread =<< findAttr (unqual "duration")     r
  let esc = case findAttr (unqual "escrow") r of
              Just x  -> mread x
              Nothing -> Nothing
  pri <-                   mread =<< findAttr (unqual "price")        r
  bid <- (/= 0)       <$> (mread =<< findAttr (unqual "bid")          r)
  iss <-                   tread =<< findAttr (unqual "issued")       r
  return (MarketOrder oid cid sid ovl nvl mvl ost tid rng aky dur esc pri bid iss)

charMedals :: APIKey k => EVEDB -> k -> CharacterID ->
              IO (LowLevelResult ([MedalAward ()],
                                  [MedalAward (CorporationID, String, String)]))
charMedals = standardRequest "char/Medals" $ \ x ->
 maybe (Left $ EVEParseError x) Right $ do
  curs <- findElementWithAttName "currentCorporation" x
  oths <- findElementWithAttName "otherCorporations"  x
  mc   <- mapChildren "row" curs parseBaseMedal
  oc   <- mapChildren "row" oths $ \ r -> do
            base <- parseBaseMedal r
            cid  <- CorpID <$> (mread =<< findAttr (unqual "corporationID") r)
            titl <-                       findAttr (unqual "title")         r
            desc <-                       findAttr (unqual "description")   r
            return base { medaExtra = (cid, titl, desc) }
  return (mc, oc)

parseBaseMedal :: Element -> Maybe (MedalAward ())
parseBaseMedal r = do
  mid <- MedalID <$> (mread =<< findAttr (unqual "medalID")  r)
  res <-                        findAttr (unqual "reason")   r
  sta <-                        findAttr (unqual "status")   r
  iid <- CharID  <$> (mread =<< findAttr (unqual "issuerID") r)
  idt <-              tread =<< findAttr (unqual "issued")   r
  return (MedalAward mid res sta iid idt ())

charSkillInTraining :: APIKey k => 
                       EVEDB -> k -> CharacterID -> 
                       IO (LowLevelResult (Maybe SkillInTraining))
charSkillInTraining = standardRequest "char/SkillInTraining" $ \ x -> do
 maybe (Left $ EVEParseError x) Right $ do
  training <- mread =<< getElementData "skillInTraining" x
  if training == 0
    then return Nothing
    else Just <$> parseSIT x
 where
  parseSIT s = do
   tid <- SkillID <$> (mread =<< getElementData "trainingTypeID"        s)
   end <-              tread =<< getElementData "trainingEndTime"       s
   sta <-              tread =<< getElementData "trainingStartTime"     s
   sps <-              mread =<< getElementData "trainingStartSP"       s
   spe <-              mread =<< getElementData "trainingDestinationSP" s
   lev <-              mread =<< getElementData "trainingToLevel"       s
   return (SkillInTraining tid sta end sps spe lev)

charSkillQueue :: APIKey k => 
                  EVEDB -> k -> CharacterID ->
                  IO (LowLevelResult [SkillInTraining])
charSkillQueue = standardRequest "char/SkillQueue" $ \ x -> do
 maybe (Left $ EVEParseError x) Right $ do
  rows <- sequence $ map parseSQRow $ findElements (unqual "row") x
  return (map snd (sortBy comp rows))
 where
  comp :: (Integer, SkillInTraining) -> (Integer, SkillInTraining) -> Ordering
  comp a b = compare (fst a) (fst b)
  parseSQRow :: Element -> Maybe (Integer, SkillInTraining)
  parseSQRow r = do
    pos <-              mread =<< findAttr (unqual "queuePosition") r
    tid <- SkillID <$> (mread =<< findAttr (unqual "typeID")        r)
    lev <-              mread =<< findAttr (unqual "level")         r
    ssp <-              mread =<< findAttr (unqual "startSP")       r
    esp <-              mread =<< findAttr (unqual "endSP")         r
    stm <-              tread =<< findAttr (unqual "startTime")     r
    etm <-              tread =<< findAttr (unqual "endTime")       r
    return (pos, SkillInTraining tid stm etm ssp esp lev) 

charStandings :: APIKey k =>
                 EVEDB -> k -> CharacterID ->
                 IO (LowLevelResult ([Standing CharacterID],
                                     [Standing CorporationID],
                                     [Standing FactionID]))
charStandings = standardRequest "char/Standings" $ \ x -> do
 maybe (Left $ EVEParseError x) Right $ do
  standsTo   <- findElement (unqual "standingsTo")       x
  standsFrom <- findElement (unqual "standingsFrom")     x
  charsTo    <- findElementWithAttName "characters"      standsTo
  corpsTo    <- findElementWithAttName "corporations"    standsTo
  agntsFrom  <- findElementWithAttName "agents"          standsFrom
  corpsFrom  <- findElementWithAttName "NPCCorporations" standsFrom
  factsFrom  <- findElementWithAttName "factions"        standsFrom
  char1      <- parseStandings charsTo   "to"   CharID (Standing GivenStanding)
  corp1      <- parseStandings corpsTo   "to"   CorpID (Standing GivenStanding)
  char2      <- parseStandings agntsFrom "from" CharID (Standing GottenStanding)
  corp2      <- parseStandings corpsFrom "from" CorpID (Standing GottenStanding)
  facts      <- parseStandings factsFrom "from" FacID  (Standing GottenStanding)
  return (char1 ++ char2, corp1 ++ corp2, facts)
 where
  parseStandings xs prefix idBuild objBuild = 
   sequence $ for (findElements (unqual "row") xs) $ \ x -> do
    sid <- idBuild <$> (mread =<< findAttr (unqual $ prefix ++ "ID")   x)
    sst <-                        findAttr (unqual $ prefix ++ "Name") x
    stn <-              mread =<< findAttr (unqual   "standing")       x
    return (objBuild sid sst stn)
  for = flip map

{-
charWalletJournal :: FullAPIKey -> CharacterID -> Maybe RefID -> IO
                     LowLevelResult
charWalletJournal = walkableRequest "char/WalletJournal" "beforeRefID"

charWalletTransactions :: FullAPIKey -> CharacterID -> Maybe RefID ->
                          IO LowLevelResult
charWalletTransactions = 
  walkableRequest "char/WalletTransactions" "beforeTransID"

charNotifications :: FullAPIKey -> CharacterID -> IO LowLevelResult
charNotifications = standardRequest "char/Notifications"
-}

corpAccountBalances :: EVEDB -> FullAPIKey -> CharacterID -> 
                       IO (LowLevelResult [(AccountID, Integer, Double)])
corpAccountBalances = 
 standardRequest "corp/AccountBalance" $ parseRows $ \ r -> do
  accountID <- AccID <$> (mread =<< findAttr (unqual "accountID")  r)
  accKey    <-            mread =<< findAttr (unqual "accountKey") r
  amount    <-            mread =<< findAttr (unqual "balance")    r
  return (accountID, accKey, amount)

corpAssetList :: EVEDB -> FullAPIKey -> CharacterID -> 
                 IO (LowLevelResult [Item])
corpAssetList = 
 extendedRequest [("version", "2")] "corp/AssetList" $ \ x ->
  maybe (Left $ EVEParseError x) Right $ do
   rs0 <- findElement (unqual "result") x
   rs1 <- findChild   (unqual "rowset") rs0
   foldChildren "row" rs1 [] $ \ acc cur -> do
     res <- parseItem Nothing cur
     return (res : acc)

{-
corpContainerLog :: FullAPIKey -> CharacterID -> IO LowLevelResult
corpContainerLog = standardRequest "corp/ContainerLog"

corporationSheet :: APIKey k => k -> CharacterID -> IO LowLevelResult
corporationSheet = standardRequest "corp/CorporationSheet"

-}

corpFactionalWarfareStats :: APIKey k =>
                             EVEDB -> k -> CharacterID ->
                             IO (LowLevelResult (Maybe CorpWarfareStats))
corpFactionalWarfareStats = standardRequest "corp/FacWarStats" $ \ x ->
  maybe (Left $ EVEParseError x) Right $ actualData x <|> notInvolved x
 where
  actualData x = do
    fid <- FacID <$> (mread =<< getElementData "factionID"   x)
    fnm <-            mread =<< getElementData "factionName" x
    enl <-            tread =<< getElementData "enlisted"    x
    pil <-            mread =<< getElementData "pilots"      x
    kst <- readKillStats x
    return (Just $ CorpWarfareStats fid fnm enl pil kst)
  notInvolved x = do
    err  <- findElement (unqual "error") x
    code <- mread =<< findAttr (unqual "code") err
    guard (code == 125)
    return Nothing


{- Skip this one for now
corpIndustryJobs :: FullAPIKey -> CharacterID -> IO LowLevelResult
corpIndustryJobs = standardRequest "corp/IndustryJobs"
-}

corpKillLogs :: EVEDB -> FullAPIKey -> CharacterID -> Maybe RefID -> 
                IO (LowLevelResult [Kill])
corpKillLogs = walkableRequest "corp/Killlog" "beforeKillID" $ \ xml ->
 maybe (Left $ EVEParseError xml) Right $ readKillLog xml

corpMarketOrders :: EVEDB -> FullAPIKey -> CharacterID ->
                    IO (LowLevelResult [MarketOrder])
corpMarketOrders = standardRequest "corp/MarketOrders" $ parseRows readOrder

corpMedals :: APIKey k =>
              EVEDB -> k -> CharacterID ->
              IO (LowLevelResult [Medal])
corpMedals = standardRequest "corp/Medals" $ parseRows $ \ r -> do
  mid <- MedalID <$> (mread =<< findAttr (unqual "medalID")     r)
  ttl <-                        findAttr (unqual "title")       r
  dsc <-                        findAttr (unqual "description") r
  crt <- CharID  <$> (mread =<< findAttr (unqual "creatorID")   r)
  crd <-              tread =<< findAttr (unqual "created")     r
  return (Medal mid ttl dsc crt crd)

corpMemberMedals :: APIKey k =>
                    EVEDB -> k -> CharacterID ->
                    IO (LowLevelResult [MedalAward ()])
corpMemberMedals =
 standardRequest "corp/MemberMedals" $ parseRows parseBaseMedal

{-
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

readKillStats :: Element -> Maybe KillTotals
readKillStats xml = do
  kyest <-  mread =<< getElementData "killsYesterday"         xml
  kweek <-  mread =<< getElementData "killsLastWeek"          xml
  ktot  <-  mread =<< getElementData "killsTotal"             xml
  vyest <-  mread =<< getElementData "victoryPointsYesterday" xml
  vweek <-  mread =<< getElementData "victoryPointsLastWeek"  xml
  vtot  <-  mread =<< getElementData "victoryPointsTotal"     xml
  return (KillTotals kyest kweek ktot vyest vweek vtot)

eveFactionalWarfareStats :: EVEDB ->
                            IO (LowLevelResult (KillTotals, [FactionStats]))
eveFactionalWarfareStats = runRequest "eve/FacWarStats" [] parse
 where
  parse xml = maybe (Left $ EVEParseError xml) Right $ do
    facs  <-    findElementWithAttName "factions"               xml
    facws <-    findElementWithAttName "factionWars"            xml
    totals <- readKillStats xml
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
                 Just (RID rid) -> [(name, show rid)]

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
  body     = intercalate "&" $ map (\ (a,b) -> a ++ "=" ++ b) args
  reqHash  = showDigest $ sha512 $ BSC.pack $ show req ++ body

getElementStringContent :: String -> Element -> Maybe String
getElementStringContent name xml = do
  elem <- findElement (unqual name) xml
  return $ strContent elem
