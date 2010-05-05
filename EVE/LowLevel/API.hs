{-# LANGUAGE ScopedTypeVariables #-}
module EVE.LowLevel.API (
         charAssetList
       , characterSheet
       , charFactionalWarfareStats
--       , charIndustryJobs
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
       , charCorporationSheet
       , corporationSheet
       , corpFactionalWarfareStats
--       , corpIndustryJobs
       , corpKillLogs
       , corpMarketOrders
       , corpMedals
       , corpMemberMedals
       , corpMemberSecurity
--       , corpMemberSecurityLog
--       , corpMemberTracking
--       , corpPOSDetails
--       , corpPOSList
--       , corpShareholders
--       , corpStandings
--       , corpTitles
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
 where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Data.Time
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

-- |Returns a complete list of the character's assets.
charAssetList :: EVEDB -> FullAPIKey -> CharacterID ->
                 IO [Item]
charAssetList =
 extendedRequest [("version", "2")] "char/AssetList" $ \ x -> do
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
  sng <- boolify   <$> (mread =<< findAttr (unqual "singleton")  el)
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
  return (Item iid (lid, flg) tid qnt (not sng) sub)

-- |Returns a character's base stats.
charFactionalWarfareStats :: APIKey k => 
                             EVEDB -> k -> CharacterID ->
                             IO (Maybe CharWarfareStats)
charFactionalWarfareStats = standardRequest "char/FacWarStats" $ \ x -> do
  actualData x <|> notInvolved x
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
    err             <- findElement (unqual "error") x
    code :: Integer <- mread =<< findAttr (unqual "code") err
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
                IO [Kill]
charKillLogs = walkableRequest "char/Killlog" "beforeKillID" readKillLog

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
      afnl <- boolify    <$> (mread =<< findAttr (unqual "finalBlow")       ar)
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
                    IO [(ListID, String)]
charMailingLists = standardRequest "char/mailinglists" $ parseRows $ \ r -> do
  lid <- ListID <$> (mread =<< findAttr (unqual "listID")      r)
  nam <-                       findAttr (unqual "displayName") r
  return (lid, nam)

charMailMessages :: EVEDB -> FullAPIKey -> CharacterID ->
                    IO [MailMessage]
charMailMessages = standardRequest "char/MailMessages" $ parseRows $ \ r -> do
  mid <- MsgID   <$> (mread =<< findAttr (unqual "messageID")          r)
  sid <- CharID  <$> (mread =<< findAttr (unqual "senderID")           r)
  dat <-              tread =<< findAttr (unqual "sentDate")           r
  tit <-                        findAttr (unqual "title")              r
  tco <-                        findAttr (unqual "toCorpOrAllianceID") r
  tli <-                        findAttr (unqual "toListIDs")          r
  tch <-                        findAttr (unqual "toCharacterIDs")     r
  rd  <- boolify <$> (mread =<< findAttr (unqual "read")               r)
  dst <- liftM3 (\ a b c -> a ++ b ++ c)
                (unwindCommas (ToCorpOrAlliance . CorpID) tco)
                (unwindCommas (ToCharacter      . CharID) tch)
                (unwindCommas (ToMailingList    . ListID) tli)
  return (MailMessage mid sid dst dat tit rd)
 where
  unwindCommas _ "" = return []
  unwindCommas f xs = do let (start,rest) = break (/= ',') xs
                         rest' <- unwindCommas f (drop 1 rest)
                         cur   <- f <$> mread start
                         return (cur:rest')

charMarketOrders :: EVEDB -> FullAPIKey -> CharacterID ->
                    IO [MarketOrder]
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
  bid <- boolify      <$> (mread =<< findAttr (unqual "bid")          r)
  iss <-                   tread =<< findAttr (unqual "issued")       r
  return (MarketOrder oid cid sid ovl nvl mvl ost tid rng aky dur esc pri bid iss)

charMedals :: APIKey k => EVEDB -> k -> CharacterID ->
              IO ([MedalAward ()],
                  [MedalAward (CorporationID, String, String)])
charMedals = standardRequest "char/Medals" $ \ x -> do
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
                       IO (Maybe SkillInTraining)
charSkillInTraining = standardRequest "char/SkillInTraining" $ \ x -> do
  training :: Integer <- mread =<< getElementData "skillInTraining" x
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
                  IO [SkillInTraining]
charSkillQueue = standardRequest "char/SkillQueue" $ \ x -> do
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
                 IO ([Standing CharacterID],
                     [Standing CorporationID],
                     [Standing FactionID])
charStandings = standardRequest "char/Standings" $ \ x -> do
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

charWalletJournal :: EVEDB -> FullAPIKey -> CharacterID -> Maybe RefID -> IO
                     [WalletJournalEntry]
charWalletJournal =
  walkableRequest "char/WalletJournal" "beforeRefID" parseWalletJournal

parseWalletJournal :: Element -> Maybe [WalletJournalEntry]
parseWalletJournal = parseRows $ \ r -> do
    dat <-             tread =<< findAttr (unqual "date")          r
    ref <- RID    <$> (mread =<< findAttr (unqual "refID")         r)
    p1i <- CharID <$> (mread =<< findAttr (unqual "ownerID1")      r)
    p1n <-                       findAttr (unqual "ownerName1")    r
    p2i <- CharID <$> (mread =<< findAttr (unqual "ownerID2")      r)
    p2n <-                       findAttr (unqual "ownerName2")    r
    amt <-             mread =<< findAttr (unqual "amount")        r
    bal <-             mread =<< findAttr (unqual "balance")       r
    typ <-             mread =<< findAttr (unqual "refTypeID")     r
    let txi = case findAttr (unqual "taxReceiverID") r of
                Nothing -> Nothing
                Just rc -> do
                  txr <- CorpID <$> mread rc
                  txa <- mread =<< findAttr (unqual "taxAmount") r
                  return (txr, txa)
    ext <- case typ :: Integer of
             1  -> PlayerTrading 
                     <$> (StatID <$> (mread =<< findAttr (unqual "argID1") r))
                     <*> (findAttr (unqual "argName1") r)
             2  -> MarketTransaction 
                     <$> (RID <$> (mread =<< findAttr (unqual "argName1") r))
             10 -> PlayerDonation
                     <$> (findAttr (unqual "reason") r)
             19 -> Insurance
                     <$> (TID <$> (mread =<< findAttr (unqual "argName1") r))
             35 -> CSPA
                     <$> (findAttr (unqual "argName1") r)
                     <*> (CharID <$> (mread =<< findAttr (unqual "argID1") r))
             37 -> CorpAccountWithdrawal
                     <$> (findAttr (unqual "reason") r)
             56 -> Manufacturing
                     <$> (JobID <$> (mread =<< findAttr (unqual "argName1") r))
             85 -> BountyPrizes
                     <$> (SSID  <$> (mread =<< findAttr (unqual "argID1") r))
             _  -> return (Unknown typ)
    return (WalletJournalEntry dat ref (p1i,p1n) (p2i,p2n) 
                               amt bal txi ext)

charWalletTransactions :: EVEDB -> FullAPIKey -> CharacterID -> Maybe RefID ->
                          IO [WalletTransaction]
charWalletTransactions =
 walkableRequest "char/WalletTransactions" "beforeTransID" parseWalletTrans

parseWalletTrans :: Element -> Maybe [WalletTransaction]
parseWalletTrans = parseRows $ \ r -> do
  td <-             tread =<< findAttr (unqual "transactionDateTime") r
  ti <- RID    <$> (mread =<< findAttr (unqual "transactionID")       r)
  qn <-             mread =<< findAttr (unqual "quantity")            r
  on <-                       findAttr (unqual "typeName")            r
  oi <- TID    <$> (mread =<< findAttr (unqual "typeID")              r)
  pr <-             mread =<< findAttr (unqual "price")               r
  ci <- CharID <$> (mread =<< findAttr (unqual "clientID")            r)
  cn <-                       findAttr (unqual "clientName")          r
  si <- StatID <$> (mread =<< findAttr (unqual "stationID")           r)
  sn <-                       findAttr (unqual "stationName")         r
  tt <-             mread =<< findAttr (unqual "transactionType")     r
  tf <-             mread =<< findAttr (unqual "transactionFor")      r
  return (WalletTransaction td ti qn (oi, on) pr (ci, cn) (si, sn) tt tf)

charNotifications :: EVEDB -> FullAPIKey -> CharacterID -> 
                     IO [Notification]
charNotifications = standardRequest "char/Notifications" $ parseRows $ \ r -> do
  ni <- NotID     <$> (mread =<< findAttr (unqual "notificationID") r)
  ti <- toNotType =<< (mread =<< findAttr (unqual "typeID")         r)
  si <- CharID    <$> (mread =<< findAttr (unqual "senderID")       r)
  sd <-                tread =<< findAttr (unqual "sentDate")       r
  rd <- boolify   <$> (mread =<< findAttr (unqual "read")           r)
  return (Notification ni ti si sd rd)

corpAccountBalances :: EVEDB -> FullAPIKey -> CharacterID -> 
                       IO [(AccountID, Integer, Double)]
corpAccountBalances = 
 standardRequest "corp/AccountBalance" $ parseRows $ \ r -> do
  accountID <- AccID <$> (mread =<< findAttr (unqual "accountID")  r)
  accKey    <-            mread =<< findAttr (unqual "accountKey") r
  amount    <-            mread =<< findAttr (unqual "balance")    r
  return (accountID, accKey, amount)

corpAssetList :: EVEDB -> FullAPIKey -> CharacterID -> 
                 IO [Item]
corpAssetList = 
 extendedRequest [("version", "2")] "corp/AssetList" $ \ x -> do
   rs0 <- findElement (unqual "result") x
   rs1 <- findChild   (unqual "rowset") rs0
   foldChildren "row" rs1 [] $ \ acc cur -> do
     res <- parseItem Nothing cur
     return (res : acc)

corpContainerLog :: EVEDB -> FullAPIKey -> CharacterID ->
                    IO [ContainerLogEntry]
corpContainerLog = standardRequest "corp/ContainerLog" $ parseRows $ \ r -> do
  lt <-             tread =<< findAttr (unqual "logTime")          r
  ii <- IID    <$> (mread =<< findAttr (unqual "itemID")           r)
  it <- TID    <$> (mread =<< findAttr (unqual "itemTypeID")       r)
  ai <- CharID <$> (mread =<< findAttr (unqual "actorID")          r)
  an <-                       findAttr (unqual "actorName")        r
  fl <-             mread =<< findAttr (unqual "flag")             r
  li <- LocID  <$> (mread =<< findAttr (unqual "locationID")       r)
  ac <-                       findAttr (unqual "action")           r
  pt <-                       findAttr (unqual "passwordType")     r
  let ti = case (findAttr (unqual "typeID") r,findAttr (unqual "quantity") r) of
             (Just i, Just q) -> do
                tid <- TID <$> mread i
                qnt <-         mread q
                return (tid, qnt) 
             _                -> Nothing
  oc <-                       findAttr (unqual "oldConfiguration") r
  nc <-                       findAttr (unqual "newConfiguration") r
  return (ContainerLogEntry lt (ii, it) (ai, an) fl li ac pt ti (oc, nc))

charCorporationSheet :: APIKey k => 
                        EVEDB -> k -> CharacterID ->
                        IO Corporation
charCorporationSheet = standardRequest "corp/CorporationSheet" parseCorpSheet

corporationSheet :: EVEDB -> CorporationID -> IO Corporation
corporationSheet db (CorpID cid) =
  runRequest "corp/CorporationSheet" [("corporationID", show cid)]
             parseCorpSheet db

parseCorpSheet :: Element -> Maybe Corporation
parseCorpSheet x = do
  ci <- CorpID     <$> (mread =<< getElementData "corporationID"   x)
  cn <-                           getElementData "corporationName" x
  tk <-                           getElementData "ticker"          x
  li <- CharID     <$> (mread =<< getElementData "ceoID"           x)
  ln <-                           getElementData "ceoName"         x
  si <- StatID     <$> (mread =<< getElementData "stationID"       x)
  sn <-                           getElementData "stationName"     x
  de <-                           getElementData "description"     x
  ur <-                           getElementData "url"             x
  let al = case AllianceID <$> (mread =<< getElementData "allianceID" x) of
             Just ai -> do
               an <- getElementData "allianceName" x
               return (ai, an)
             Nothing ->
               Nothing
  tr <-                 mread =<< getElementData "taxRate"         x
  mc <-                 mread =<< getElementData "memberCount"     x
  let ml = case mread =<< getElementData "memberLimit" x of
             Just lim -> Just lim
             Nothing  -> Nothing
  sh <-                 mread =<< getElementData "shares"          x
  let divs = case findElementWithAttName "divisions" x of
               Just rs -> mapChildren "row" rs $ \ r -> do
                 ak <- AccID <$> (mread =<< findAttr (unqual "accountKey")  r)
                 an <-                      findAttr (unqual "description") r
                 return (ak, an)
               Nothing ->
                 Nothing
      wals = case findElementWithAttName "walletDivisions" x of
               Just rs -> mapChildren "row" rs $ \ r -> do
                 ak <- WDID <$> (mread =<< findAttr (unqual "accountKey")  r)
                 an <-                     findAttr (unqual "description") r
                 return (ak, an)
               Nothing ->
                 Nothing
  return (Corporation ci cn tk (li, ln) (si, sn) de ur al tr mc ml sh divs wals) 

corpFactionalWarfareStats :: APIKey k =>
                             EVEDB -> k -> CharacterID ->
                             IO (Maybe CorpWarfareStats)
corpFactionalWarfareStats = standardRequest "corp/FacWarStats" $ \ x ->
  actualData x <|> notInvolved x
 where
  actualData x = do
    fid <- FacID <$> (mread =<< getElementData "factionID"   x)
    fnm <-            mread =<< getElementData "factionName" x
    enl <-            tread =<< getElementData "enlisted"    x
    pil <-            mread =<< getElementData "pilots"      x
    kst <- readKillStats x
    return (Just $ CorpWarfareStats fid fnm enl pil kst)
  notInvolved x = do
    err             <- findElement (unqual "error") x
    code :: Integer <- mread =<< findAttr (unqual "code") err
    guard (code == 125)
    return Nothing


{- Skip this one for now
corpIndustryJobs :: FullAPIKey -> CharacterID -> IO LowLevelResult
corpIndustryJobs = standardRequest "corp/IndustryJobs"
-}

corpKillLogs :: EVEDB -> FullAPIKey -> CharacterID -> Maybe RefID -> 
                IO [Kill]
corpKillLogs = walkableRequest "corp/Killlog" "beforeKillID" $ \ xml ->
 readKillLog xml

corpMarketOrders :: EVEDB -> FullAPIKey -> CharacterID ->
                    IO [MarketOrder]
corpMarketOrders = standardRequest "corp/MarketOrders" $ parseRows readOrder

corpMedals :: APIKey k =>
              EVEDB -> k -> CharacterID ->
              IO [Medal]
corpMedals = standardRequest "corp/Medals" $ parseRows $ \ r -> do
  mid <- MedalID <$> (mread =<< findAttr (unqual "medalID")     r)
  ttl <-                        findAttr (unqual "title")       r
  dsc <-                        findAttr (unqual "description") r
  crt <- CharID  <$> (mread =<< findAttr (unqual "creatorID")   r)
  crd <-              tread =<< findAttr (unqual "created")     r
  return (Medal mid ttl dsc crt crd)

corpMemberMedals :: APIKey k =>
                    EVEDB -> k -> CharacterID ->
                    IO [MedalAward ()]
corpMemberMedals =
 standardRequest "corp/MemberMedals" $ parseRows parseBaseMedal

corpMemberSecurity :: EVEDB -> FullAPIKey -> CharacterID -> 
                      IO [CorpMemberSecurity]
corpMemberSecurity = standardRequest "corp/MemberSecurity" $ \ v -> do
  mapElements "member" v $ \ x -> do
   chi <- CharID <$> (mread =<< findAttr (unqual "characterID") x)
   chn <-                       findAttr (unqual "name")        x
   tls <- do rs <- findElementWithAttName "titles" x
             mapElements "row" rs $ \ r -> do
               tid <- TitleID <$> (mread =<< findAttr (unqual "titleID")   r)
               tnm <-                        findAttr (unqual "titleName") r
               return (CorpTitle tid tnm)
   rls <- foldM (runRole x) Map.empty roleMap
   return (CorpMemberSecurity chi chn (Map.elems rls) tls)
 where
  fixOld f' _ (CorpRole i n fs) = CorpRole i n (f':fs)
  updateVal k n f m    = Map.insertWith (fixOld f) k (CorpRole k n [f]) m
  addRoleEntries f m e = foldElements "row" e m $ \ a r -> do
    i <- RoleID <$> (mread =<< findAttr (unqual "roleID")   r)
    n <-                       findAttr (unqual "roleName") r
    return (updateVal i n f a)
  runRole el dict (name,flag) = do
    rs <- findElementWithAttName name el
    if null (elChildren rs)
      then return dict
      else addRoleEntries flag dict rs
  roleMap = [ ("roles",                 GeneralRole)
            , ("grantableRoles",        GrantableRole)
            , ("rolesAtHQ",             AtHQ)
            , ("grantableRolesAtHQ",    GrantableAtHQ)
            , ("rolesAtBase",           AtBase)
            , ("grantableRolesAtBase",  GrantableAtBase)
            , ("rolesAtOther",          Elsewhere)
            , ("grantableRolesAtOther", GrantableElsewhere)
            ]

{-
corpMemberSecurityLog :: FullAPIKey -> CharacterID -> IOLowLevelResult
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
-}

corpWalletJournal :: EVEDB -> FullAPIKey -> CharacterID -> Maybe RefID ->
                     IO [WalletJournalEntry]
corpWalletJournal = 
  walkableRequest "corp/WalletJournal" "beforeRefID" parseWalletJournal

corpWalletTransactions :: EVEDB -> FullAPIKey -> CharacterID -> Maybe RefID ->
                          IO [WalletTransaction]
corpWalletTransactions =
  walkableRequest "corp/WalletTransactions" "beforeTransID" parseWalletTrans

eveAllianceList :: EVEDB -> IO [Alliance]
eveAllianceList = runRequest "eve/AllianceList" [] parse
 where
  parse xml = do
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

eveConquerableStationList :: EVEDB -> IO [ConquerableStation]
eveConquerableStationList =
  runRequest "eve/ConquerableStationList" [] $ parseRows $ \ r -> do
    sID <- StatID <$> (mread =<< findAttr (unqual "stationID")       r)
    sNm <-                       findAttr (unqual "stationName")     r
    sTp <-             mread =<< findAttr (unqual "stationTypeID")   r
    sSS <- SSID   <$> (mread =<< findAttr (unqual "solarSystemID")   r)
    cID <- CorpID <$> (mread =<< findAttr (unqual "corporationID")   r)
    cNm <-                       findAttr (unqual "corporationName") r
    return (CStat sID sNm sTp sSS cID cNm)

eveErrorList :: EVEDB -> IO [(Integer,String)]
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
                            IO (KillTotals, [FactionStats])
eveFactionalWarfareStats = runRequest "eve/FacWarStats" [] parse
 where
  parse xml = do
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

eveFactionalWarfareTop100 :: EVEDB -> IO KillStats
eveFactionalWarfareTop100 = runRequest "eve/FacWarTopStats" [] parse
 where
  parse xml = do
    chs       <- findElement (unqual "characters")   xml
    cps       <- findElement (unqual "corporations") xml
    fas       <- findElement (unqual "factions")     xml
    charStats <- processStats "characterID"   "characterName"   CharID chs
    corpStats <- processStats "corporationID" "corporationName" CorpID cps
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
  toList idAtt nameAtt valAtt builder xml = mapChildren "row" xml $ \ row -> do
    gid   <- builder <$> (mread =<< findAttr (unqual idAtt)   row)
    name  <-                        findAttr (unqual nameAtt) row
    val   <-              mread =<< findAttr (unqual valAtt)  row
    return (gid, name, val)

eveIDToName :: [CharacterID] -> EVEDB ->
               IO [(String,CharacterID)]
eveIDToName ids =
  runRequest "eve/CharacterName" [("Ids",ids')] readNameIDs
 where ids' = intercalate "," $ map (\ (CharID x) -> show x) ids

eveNameToID :: [String] -> EVEDB -> 
               IO [(String,CharacterID)]
eveNameToID names =
  runRequest "eve/CharacterID" [("names",names')] readNameIDs
 where names'    = intercalate "," names

readNameIDs :: Element -> Maybe [(String,CharacterID)]
readNameIDs = parseRows $ \ r -> do
  name <-           findAttr (unqual "name")        r
  cid  <- mread =<< findAttr (unqual "characterID") r
  return (name, CharID cid)

eveRefTypesList :: EVEDB -> IO [(Integer,String)]
eveRefTypesList = runRequest "eve/RefTypes" [] $ parseRows readRow
 where
  readRow r = do
    refid   <- mread =<< findAttr (unqual "refTypeID")   r
    refname <-           findAttr (unqual "refTypeName") r
    return (refid, refname)

mapFactionalWarfareOccupancyMap
  :: EVEDB -> IO [(Integer, String, Integer, String, Bool)]
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
mapJumps :: EVEDB -> IO [(Integer, Integer)]
mapJumps = runRequest "map/Jumps" [] $ parseRows readRow
 where
  readRow row = do
    sid    <- mread =<< findAttr (unqual "solarSystemID") row
    jumps  <- mread =<< findAttr (unqual "shipJumps") row
    return (sid, jumps)

-- |Returns a list (solarSystemID, shipKills, factionKills, podKills)
mapKills :: EVEDB -> IO [(Integer, Integer, Integer, Integer)]
mapKills = runRequest "map/Kills" [] $ parseRows readRow
 where
  readRow :: Element -> Maybe (Integer, Integer, Integer, Integer)
  readRow row = do
    sid    <- mread =<< findAttr (unqual "solarSystemID") row
    shipKs <- mread =<< findAttr (unqual "shipKills")     row
    facKs  <- mread =<< findAttr (unqual "factionKills")  row
    podKs  <- mread =<< findAttr (unqual "podKills")      row
    return (sid, shipKs, facKs, podKs)

mapSovereignty :: EVEDB -> IO [SolarSystem]
mapSovereignty = runRequest "map/Sovereignty" [] $ parseRows readRow
 where
  readRow :: Element -> Maybe SolarSystem
  readRow row = do
    sid    <- SSID       <$> (mread =<< findAttr (unqual "solarSystemID")   row)
    name   <-           findAttr (unqual "solarSystemName") row
    aliID  <- AllianceID <$> (mread =<< findAttr (unqual "allianceID")      row)
    corID  <- CorpID     <$> (mread =<< findAttr (unqual "corporationID")   row)
    facID  <- FacID      <$> (mread =<< findAttr (unqual "factionID")       row)
    let list0 = []
        list1 = if aliID == noAll  then list0 else (OwnerAlliance aliID : list0)
        list2 = if corID == noCorp then list1 else (OwnerCorp     corID : list1)
        list3 = if facID == noFac  then list2 else (OwnerFaction  facID : list2)
    return $ SolarSystem sid name list3

serverStatus :: EVEDB -> IO (Bool, Integer)
serverStatus = runRequest "server/ServerStatus" [] pullResult
 where
  pullResult xml = do
    open    <- mread =<< getElementStringContent "serverOpen" xml
    players <- mread =<< getElementStringContent "onlinePlayers" xml
    return (open, players)

-----------------------------------------------------------------------------
-- Some helper functions to make writing the above less tedious.
--

