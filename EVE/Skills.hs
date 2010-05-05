module EVE.Skills(
         SkillID
       , CertificateID
       , updateSkillTree
       , updateCertificateTrees
       )
 where

import Control.Arrow(first)
import Control.Monad(unless)
import EVE.Monad.Internal
import EVE.LowLevel.DB
import Text.XML.Light
import Text.XML.Light.Helpers

newtype SkillID = SkillID Integer

instance Read SkillID where
  readsPrec p s = map (first SkillID) (readsPrec p s)

instance Show SkillID where
  show (SkillID x) = "skill:" ++ show x

newtype CertificateID = CertID Integer

instance Read CertificateID where
  readsPrec p s = map (first CertID) (readsPrec p s)

instance Show CertificateID where
  show (CertID x) = "cert:" ++ show x

-- |Update the saved skill tree from the database's servers.
updateSkillTree :: APIKey k => EVE k ()
updateSkillTree = do
  db            <- getDB
  (sg,sk,sr,sb) <- runRequest "eve/SkillTree" [] parseSkills
  runIO $ updateSkillTables db sg sk sr sb
 where
  parseSkills :: Element -> Maybe
                 ([(Integer,  String)],
                  [(Integer,  Integer,   String,        String,
                    String,   String,    Integer)],
                  [(Integer,  Integer,   Integer)],
                  [(Integer,  String,    Maybe Integer, Maybe Float)])
  parseSkills xml = do
    rows <- findElementWithAttName "skillGroups" xml
    foldChildren "row" rows ([], [], [], [])$ \ (grs1,sks1,srs1,sbs1) grow -> do
      name <- findAttr (unqual "groupName") grow
      gid  <- mread =<< findAttr (unqual "groupID") grow
      let grs1' = (gid, name) : grs1
      foldChildren "rowset" grow (grs1',sks1,srs1,sbs1) $ \ acc srows ->
        foldChildren "row" srows acc $ \ (grs2,sks2,srs2,sbs2) srow -> do
          sname <-           findAttr (unqual "typeName") srow
          tid   <- mread =<< findAttr (unqual "typeID") srow
          grp   <- mread =<< findAttr (unqual "groupID") srow
          desc  <-           getChildData "description" srow
          rank  <- mread =<< getChildData "rank" srow
          pri   <-           getElementData "primaryAttribute" srow
          sec   <-           getElementData "secondaryAttribute" srow
          unless (grp == gid) $ fail "group is not equal to id!"
          let sks2' = (tid, grp, sname, desc, pri, sec, rank) : sks2
          srs2' <- findChildWithAttName "requiredSkills" srow >>= \ rreqs ->
                     foldChildren "row" rreqs srs2 $ \ srs2' req -> do
                       reqtid <- mread =<< findAttr (unqual "typeID") req
                       reqsl  <- mread =<< findAttr (unqual "skillLevel") req
                       return $! (tid, reqtid, reqsl) : srs2'
          sbs2' <- findChildWithAttName "skillBonusCollection" srow >>= \ rbs ->
                     foldChildren "row" rbs sbs2 $ \ sbs2' bon -> do
                        btype <- findAttr (unqual "bonusType") bon
                        bval  <- findAttr (unqual "bonusValue") bon
                        (vali, valf) <- readBonusVal btype bval
                        return $! (tid, btype, vali, valf) : sbs2'
          return (grs2, sks2', srs2', sbs2')

readBonusVal :: String -> String -> Maybe (Maybe Integer, Maybe Float) 
readBonusVal name valstr
  | name `elem` floatVals = return (Nothing,      mread valstr)
  | name `elem` intVals   = return (mread valstr, Nothing)
  | name `elem` noVals    = return (Nothing,      Nothing)
  | otherwise             = return (Nothing,      Nothing) -- fail "Not a recognized bonus value"
 where
  noVals    = ["canNotBeTrainedOnTrial", "charismaBonus", "intelligenceBonus",
               "memoryBonus", "perceptionBonus", "willpowerBonus"]
  floatVals = ["connectionBonusMutator","criminalConnectionsMutator",
               "diplomacyMutator", "uniformityBonus"]
  intVals   = ["accessDifficultyBonus", "agilityBonus", "aoeCloudSizeBonus",
               "aoeVelocityBonus", "armorHpBonus",
               "blueprintmanufactureTimeBonus", "boosterAttributeModifier",
               "boosterChanceBonus", "bountySkillBonus", "capNeedBonus",
               "capRechargeBonus", "capacitorCapacityBonus",
               "capacitorNeedMultipler", "cloakingTargetingDelayBonus",
               "consumptionQuantityBonus", "consumptionQuantityBonusPercentage",
               "copySpeedBonus", "corporationMemberBonus", "cpuNeedBonus",
               "cpuOutputBonus2", "damageCloudChanceReduction", "damageHP",
               "damageMultiplierBonus", "droneMaxVelocityBonus",
               "droneRangeBonus", "durationBonus", "durationSkillBonus",
               "falloffBonus", "fastTalkMutator", "hardeningBonus",
               "hardeningbonus2", "hullHpBonus", "iceHarvestCycleBonus",
               "inventionBonus", "jumpDriveCapacitorNeedBonus",
               "jumpDriveRangeBonus", "laboratorySlotsBonus", "learningBonus",
               "manufactureCostBonus", "manufacturingSlotBonus",
               "manufacturingTimeBonus", "maxActiveDroneBonus",
               "maxAttackTargets", "maxFlightTimeBonus", "maxJumpClones",
               "maxJumpClonesBonus", "maxScanDeviationModifier",
               "maxTargetBonus", "maxTargetRangeBonus",
               "mineralNeedResearchBonus", "miningAmountBonus",
               "miningUpgradeCPUReductionBonus", "minmatarTechMutator",
               "missileVelocityBonus", "moduleRepairRateBonus",
               "negotiationBonus", "nonRaceCorporationMembersBonus",
               "posStructureControlAmount", "powerEngineeringOutputBonus",
               "powerNeedBonus", "projECMDurationBonus", "rangeSkillBonus",
               "rechargeratebonus", "refiningYieldMutator",
               "researchGangSizeBonus", "resistanceBonus", "rigDrawbackBonus",
               "rofBonus", "scanResolutionBonus", "scanSkillEwStrengthBonus",
               "scanSkillTargetPaintStrengthBonus", "scanStrengthBonus",
               "scanspeedBonus", "shieldBoostCapacitorBonus",
               "shieldCapacityBonus", "shieldRechargerateBonus",
               "shipBrokenRepairCostMultiplierBonus", "shipPowerBonus",
               "socialBonus", "socialMutator", "speedFBonus", "speedFactor",
               "squadronCommandBonus", "thermodynamicsHeatDamage",
               "trackingSpeedBonus", "tradePremiumBonus", "turretSpeeBonus",
               "velocityBonus", "warpCapacitorNeedBonus" ]

updateCertificateTrees :: APIKey k => EVE k ()
updateCertificateTrees = do
  db                 <- getDB
  (cc,cl,cg,crs,crc) <- runRequest "eve/CertificateTree" [] parse
  runIO $ updateCertificateTables db cc cl cg crs crc
 where
  parse :: Element ->
           Maybe ([(String, Integer)]
                 ,[(Integer, Integer, String)]
                 ,[(Integer, Integer, Integer, Integer, String)]
                 ,[(Integer, Integer, Integer)]
                 ,[(Integer, Integer)]
                 )
  parse xml = do
    rows <- findElementWithAttName "categories" xml
    foldChildren "row" rows ([],[],[],[],[]) $ \ (cc0,cl0,cg0,crs0,crc0) c -> do
      catID <- mread =<< findAttr (unqual "categoryID")   c
      catNm <-           findAttr (unqual "categoryName") c
      cset  <-           findChildWithAttName "classes"   c
      let cc0' = (catNm, catID) : cc0
      foldChildren "row" cset (cc0',cl0,cg0,crs0,crc0) $
        \ (cc1,cl1,cg1,crs1,crc1) l -> do
          clID   <- mread =<< findAttr (unqual "classID")   l
          clName <-           findAttr (unqual "className") l
          ceset  <- findChildWithAttName "certificates" l
          let cl1' = (catID, clID, clName) : cl1
          foldChildren "row" ceset (cc1,cl1',cg1,crs1,crc1) $
            \ (cc2,cl2,cg2,crs2,crc2) x -> do
              ceID    <- mread =<< findAttr (unqual "certificateID") x
              ceGrade <- mread =<< findAttr (unqual "grade")         x
              ceCorp  <- mread =<< findAttr (unqual "corporationID") x
              ceDesc  <-           findAttr (unqual "description")   x
              let cg2' = (ceID, ceGrade, clID, ceCorp, ceDesc) : cg2
              sreqs   <- case findChildWithAttName "requiredSkills" x of
                           Nothing   -> Just []
                           Just sset ->
                             mapChildren "row" sset $ \ r -> do
                               sid <- mread =<< findAttr (unqual "typeID") r
                               slv <- mread =<< findAttr (unqual "level")  r
                               return $ (ceID, sid, slv)
              creqs   <- case findChildWithAttName "requiredCertificates" x of
                           Nothing   -> Just []
                           Just rset ->
                             mapChildren "row" rset $ \ r -> do
                               cid <- mread =<<
                                         findAttr (unqual "certificateID") r
                               return $ (ceID, cid)
              return (cc2, cl2, cg2', sreqs ++ crs2, creqs ++ crc2)


 
