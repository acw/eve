{-# LANGUAGE DeriveDataTypeable #-}
module EVE.LowLevel.Types
 where

import Control.Exception
import Data.Char
import Data.List
import Data.Maybe
import Data.Time.Clock(UTCTime(..))
import Data.Time.Format()
import Data.Typeable

-----------------------------------------------------------------------------
-- Data types / classes for API keys
--

-----------------------------------------------------------------------------
-- Errors and low-level types
--

newtype CorporationID    = CorpID Integer deriving (Show,Eq)
newtype FactionID        = FacID  Integer deriving (Show,Ord,Eq)
newtype StationID        = StatID Integer deriving (Show,Ord,Eq)
newtype SolarSystemID    = SSID   Integer deriving (Show,Ord,Eq)
newtype LocationID       = LocID  Integer deriving (Show,Ord,Eq)
newtype ListID           = ListID Integer deriving (Show)
newtype MessageID        = MsgID  Integer deriving (Show)
newtype JobID            = JobID  Integer deriving (Show)

instance Read FactionID where
  readsPrec d s = map (\ (a,b) -> (FacID a,b)) $ readsPrec d s

noCorp :: CorporationID
noCorp = CorpID 0

noFac :: FactionID
noFac = FacID 0

-----------------------------------------------------------------------------
-- Skills and such
--

newtype SkillID      = SkillID Integer      deriving (Eq,Show,Ord)
newtype SkillGroupID = SkillGroupID Integer deriving (Eq,Show,Ord)

data SkillGroup = SkillGroup {
                    groupName :: String
                  , groupID   :: SkillGroupID
                  }
 deriving (Show)

data Skill = Skill {
               skillName         :: String
             , skillGroup        :: SkillGroupID
             , skillID           :: SkillID
             , skillDescription  :: String
             , skillRank         :: Integer
             , skillPrimary      :: Attribute
             , skillSecondary    :: Attribute
             , skillRequirements :: [SkillLevel]
             , skillBonuses      :: [Bonus]
             }
 deriving (Show)

data SkillInTraining = SkillInTraining {
    trainSkillID   :: SkillID
  , trainStartTime :: UTCTime
  , trainEndTime   :: UTCTime
  , trainStartSP   :: Integer
  , trainEndSP     :: Integer
  , trainLevel     :: Integer
  }
 deriving (Show)

data Bonus = AccessDifficulty Integer
           | Agility Integer
           | AreaOfEffectSize Integer
           | AreaOfEffectVelocity Integer
           | ArmorHitPoints Integer
           | BlueprintManufactureTime Integer
           | BoosterAttributeModifier Integer
           | BoosterChance Integer
           | BountySkill Integer
           | CanNotBeTrainedOnTrial
           | CapNeed Integer
           | CapRecharge Integer
           | CapacitorCapacity Integer
           | CapacitorNeedMult Integer
           | CharismaBonus
           | CloakingTargetingDelay Integer
           | ConsumptionQuantity Integer
           | ConsumptionQuantityPerc Integer
           | CopySpeed Integer
           | CorporationMember Integer
           | CPUNeed Integer
           | CPUOutputBonus Integer
           | DamageCloudChanceReduction Integer
           | DamageHP Integer
           | DamageMultiplier Integer
           | DroneMaxVelocity Integer
           | DroneRange Integer
           | Duration Integer
           | DurationSkill Integer
           | FalloffBonus Integer
           | FastTalkMutator Integer
           | Hardening Integer
           | Hardening2 Integer
           | HullHP Integer
           | IceHarvestCycle Integer
           | IntelligenceBonus
           | Invention Integer
           | JumpDriveCapacitorNeed Integer
           | JumpDriveRange Integer
           | LaboratorySlots Integer
           | Learning Integer
           | ManufactureCost Integer
           | ManufactureSlot Integer
           | ManufactureTime Integer
           | MaxActiveDrones Integer
           | MaxAttackTargets Integer
           | MaxFlightTime Integer
           | MaxJumpClones Integer
           | MaxJumpClones2 Integer
           | MaxScanDeviationModifier Integer
           | MaxTargetBonus Integer
           | MaxTargetRange Integer
           | MemoryBonus
           | MineralNeedResearch Integer
           | MiningAmount Integer
           | MiningUpgradeCPUReduction Integer
           | MinmatarTechMutator Integer
           | MissileVelocity Integer
           | ModuleRepairRate Integer
           | Negotiation Integer
           | NonRaceCorporationMembers Integer
           | PerceptionBonus
           | POSStructureControl Integer
           | PowerEngineeringOutput Integer
           | PowerNeed Integer
           | ProjECMDuration Integer
           | Range Integer
           | RechargeRate Integer
           | RefiningYieldMutator Integer
           | ResearchGangSize Integer
           | Resistance Integer
           | RigDrawback Integer
           | RateOfFire Integer
           | ScanResolution Integer
           | ScanEWStrength Integer
           | TargetPaintStrength Integer
           | ScanStrength Integer
           | ScanSpeed Integer
           | ShieldBoostCapacitor Integer
           | ShieldCapacity Integer
           | ShieldRecharge Integer
           | ShipBrokenRepCostMult Integer
           | ShipPower Integer
           | Social Integer
           | SocialMutator Integer
           | SpeedF Integer
           | SpeedFactor Integer
           | SquadronCommand Integer
           | ThermodynamicsHeatDamage Integer
           | TrackingSpeed Integer
           | TradePremium Integer
           | TurretSpeed Integer
           | Velocity Integer
           | WarpCapacitorNeed Integer
           | WillpowerBonus
           | UnknownBonus String String
 deriving (Show)

parseBonuses :: [(String, String)] -> ([SkillLevel], [Bonus])
parseBonuses ls = (mapMaybe getSkill ["4","5","6"], map (uncurry pb) others)
 where
  (reqSkills, others) = partition (isPrefixOf "requiredSkill" . fst) ls
  getSkill x = do
    skill <- lookup ("requiredSkill" ++ x)            reqSkills
    level <- lookup ("requiredSkill" ++ x ++ "Level") reqSkills
    return $ SkillLevel (SkillID $ read skill) (read level)

-----------------------------------------------------------------------------
-- Certificates
--

newtype CertificateID  = CertID   Integer deriving (Eq,Show,Ord)
newtype CertCategoryID = CCatID   Integer deriving (Eq,Show,Ord)
newtype CertClassID    = CClassID Integer deriving (Eq,Show,Ord)

data CertReq = SkillReq       SkillLevel
             | CertificateReq CertLevel
 deriving (Show)

data CertLevel   = CertLevel {
                     clCert  :: CertificateID
                   , clLevel :: Integer
                   }
 deriving (Show)

data CertificateCategory = CCat {
                             ccatID   :: CertCategoryID
                           , ccatName :: String
                           }
 deriving (Show)

data CertificateClass = CClass {
                          cclassID   :: CertClassID
                        , cclassName :: String
                        }
 deriving (Show)

data Certificate = Certificate {
                     certID           :: CertificateID
                   , certCategoryID   :: CertCategoryID
                   , certClassID      :: CertClassID
                   , certCorporation  :: CorporationID
                   , certDescription  :: String
                   , certGrade        :: Integer
                   , certRequirements :: [CertReq]
                   }
 deriving (Show)


-----------------------------------------------------------------------------
-- Map data
--

data OwnerInfo = OwnerAlliance AllianceID
               | OwnerCorp     CorporationID
               | OwnerFaction  FactionID
 deriving (Show, Eq)

data SolarSystem = SolarSystem {
       solarSystemID         :: SolarSystemID
     , solarSystemName       :: String
     , solarSystemOwner      :: [OwnerInfo]
     }
 deriving (Show, Eq)


-----------------------------------------------------------------------------
-- Kill Statistics
--

data KillStats = KillStats {
    ksCharStats :: KillList CharacterID
  , ksCorpStats :: KillList CorporationID
  , ksFacStats  :: KillList FactionID
  }
 deriving (Show)

data Show a => KillList a = KillList {
    klKillsYesterday         :: [(a, String, Integer)]
  , klKillsLastWeek          :: [(a, String, Integer)]
  , klKillsTotal             :: [(a, String, Integer)]
  , klVictoryPointsYesterday :: [(a, String, Integer)]
  , klVictoryPointsLastWeek  :: [(a, String, Integer)]
  , klVictoryPointsTotal     :: [(a, String, Integer)]
  }
 deriving (Show)

data KillTotals = KillTotals {
    ktKillsYesterday         :: Integer
  , ktKillsLastWeek          :: Integer
  , ktKillsTotal             :: Integer
  , ktVictoryPointsYesterday :: Integer
  , ktVictoryPointsLastWeek  :: Integer
  , ktVictoryPointsTotal     :: Integer
  }
 deriving (Show)


-----------------------------------------------------------------------------
-- Faction / Character / Corporation Statistics
--

data FactionStats = FactionStats {
    facFactionID         :: FactionID
  , facName              :: String
  , facPilots            :: Integer
  , facSystemsControlled :: Integer
  , facKillList          :: KillTotals
  , facAtWarWith         :: [(FactionID, String)]
  }
 deriving (Show)


data CharWarfareStats = CharWarfareStats {
    charwFactionID        :: FactionID
  , charwFactionName      :: String
  , charwEnlisted         :: UTCTime
  , charwRank             :: Integer
  , charwHighestRank      :: Integer
  , charwKillStats        :: KillTotals
  }
 deriving (Show)

data CorpWarfareStats = CorpWarfareStats {
    corpwFactionID        :: FactionID
  , corpwFactionName      :: String
  , corpwEnlisted         :: UTCTime
  , corpwPilots           :: Integer
  , coprwKillStats        :: KillTotals
  }
 deriving (Show)

-----------------------------------------------------------------------------
-- Conquerable Station List
--

data ConquerableStation = CStat {
    cstatID              :: StationID
  , cstatName            :: String
  , cstatTypeID          :: Integer
  , cstatSolarSystemID   :: SolarSystemID
  , cstatCorporationID   :: CorporationID
  , cstatCorporationName :: String
  }
 deriving (Show)

-----------------------------------------------------------------------------
-- Conquerable Station List
--

newtype AllianceID = AllianceID Integer deriving (Eq,Show,Ord)

noAll :: AllianceID
noAll = AllianceID 0

data Alliance = Alliance {
    allAlliaceID  :: AllianceID
  , allName       :: String
  , allShortName  :: String
  , allExecutor   :: CorporationID
  , allNumMembers :: Integer
  , allStart      :: UTCTime
  , allMembers    :: [(CorporationID, UTCTime)]
  }
 deriving (Show)

-----------------------------------------------------------------------------
-- Accounts
--

newtype AccountID = AccID Integer deriving (Eq, Show, Ord)

-----------------------------------------------------------------------------
-- Item Information
--

newtype ItemID           = IID Integer deriving (Eq, Show)
newtype TypeID           = TID Integer deriving (Eq, Show)

data Item = Item {
    itemID       :: ItemID
  , itemLocation :: (LocationID, LocationFlag)
  , itemType     :: TypeID
  , itemQuantity :: Integer
  , itemPackaged :: Bool
  , itemContains :: [Item]
  }
 deriving (Show)

data LocationFlag =
    None
  | Wallet
  | Factory
  | Hanger
  | Cargo
  | Briefcase
  | SkillFlag
  | Reward
  | Connected
  | Disconnected
  | LowSlot Int
  | MidSlot Int
  | HighSlot Int
  | FixedSlot
  | Capsule
  | Pilot
  | Passenger
  | BoardingGate
  | Crew
  | LocSkillInTraining
  | CorpMarket
  | Locked
  | Unlocked
  | OfficeSlot Int
  | Bonus
  | DroneBay
  | Booster
  | Implant
  | ShipHanger
  | ShipOffline
  | RigSlot Int
  | FactoryOperation
  | CorpSecurityAccessGroup Int
  | SecondaryStorage
 deriving (Eq, Show)

toLocFlag :: Int -> Maybe LocationFlag
toLocFlag   0 = Just None
toLocFlag   1 = Just Wallet
toLocFlag   2 = Just Factory
toLocFlag   4 = Just Hanger
toLocFlag   5 = Just Cargo
toLocFlag   6 = Just Briefcase
toLocFlag   7 = Just SkillFlag
toLocFlag   8 = Just Reward
toLocFlag   9 = Just Connected
toLocFlag  10 = Just Disconnected
toLocFlag  35 = Just FixedSlot
toLocFlag  56 = Just Capsule
toLocFlag  57 = Just Pilot
toLocFlag  58 = Just Passenger
toLocFlag  59 = Just BoardingGate
toLocFlag  60 = Just Crew
toLocFlag  61 = Just LocSkillInTraining
toLocFlag  62 = Just CorpMarket
toLocFlag  63 = Just Locked
toLocFlag  64 = Just Unlocked
toLocFlag  86 = Just Bonus
toLocFlag  87 = Just DroneBay
toLocFlag  88 = Just Booster
toLocFlag  89 = Just Implant
toLocFlag  90 = Just ShipHanger
toLocFlag  91 = Just ShipOffline
toLocFlag 100 = Just FactoryOperation
toLocFlag 122 = Just SecondaryStorage
toLocFlag   x
  | (x >= 11)  && (x <= 18)  = Just $ LowSlot                 (x - 10)
  | (x >= 19)  && (x <= 26)  = Just $ MidSlot                 (x - 18)
  | (x >= 27)  && (x <= 34)  = Just $ HighSlot                (x - 26)
  | (x >= 70)  && (x <= 85)  = Just $ OfficeSlot              (x - 69)
  | (x >= 92)  && (x <= 99)  = Just $ RigSlot                 (x - 91)
  | (x >= 116) && (x <= 121) = Just $ CorpSecurityAccessGroup (x - 114)
  | otherwise                = Nothing

fromLocFlag :: LocationFlag -> Int
fromLocFlag None                        = 0
fromLocFlag Wallet                      = 1
fromLocFlag Factory                     = 2
fromLocFlag Hanger                      = 4
fromLocFlag Cargo                       = 5
fromLocFlag Briefcase                   = 6
fromLocFlag SkillFlag                   = 7
fromLocFlag Reward                      = 8
fromLocFlag Connected                   = 9
fromLocFlag Disconnected                = 10
fromLocFlag (LowSlot x)                 = 10 + x
fromLocFlag (MidSlot x)                 = 18 + x
fromLocFlag (HighSlot x)                = 26 + x
fromLocFlag FixedSlot                   = 35
fromLocFlag Capsule                     = 56
fromLocFlag Pilot                       = 57
fromLocFlag Passenger                   = 58
fromLocFlag BoardingGate                = 59
fromLocFlag Crew                        = 60
fromLocFlag LocSkillInTraining          = 61
fromLocFlag CorpMarket                  = 62
fromLocFlag Locked                      = 63
fromLocFlag Unlocked                    = 64
fromLocFlag (OfficeSlot x)              = 69 + x
fromLocFlag Bonus                       = 86
fromLocFlag DroneBay                    = 87
fromLocFlag Booster                     = 88
fromLocFlag Implant                     = 89
fromLocFlag ShipHanger                  = 90
fromLocFlag ShipOffline                 = 91
fromLocFlag (RigSlot x)                 = 91 + x
fromLocFlag FactoryOperation            = 100
fromLocFlag (CorpSecurityAccessGroup x) = 114 + x
fromLocFlag SecondaryStorage            = 122

-----------------------------------------------------------------------------
-- Character Information
--
-----------------------------------------------------------------------------
-- Kill Data
--

data Kill = Kill {
    killID           :: RefID
  , killTime         :: UTCTime
  , killLocation     :: SolarSystemID
  , killVictim       :: (CharacterID, String, CorporationID, String, AllianceID)
  , killDamageTaken  :: Integer
  , killShipType     :: TypeID
  , killAttackers    :: [AttackerInfo]
  , killItemResults  :: [(TypeID, LocationFlag, Integer, Integer)]
  }
 deriving (Show)

data AttackerInfo = AttackerInfo {
    attBase           :: (CharacterID, String,
                          CorporationID, String,
                          AllianceID, String)
  , attSecurityStatus :: Float
  , attDamageDone     :: Integer
  , attFinalBlow      :: Bool
  , attWeapon         :: TypeID
  , attShip           :: TypeID
  }
 deriving (Show)

-----------------------------------------------------------------------------
-- Mail Messages and Notifications
--

data MailMessage = MailMessage {
    msgID          :: MessageID
  , msgSender      :: CharacterID
  , msgDestination :: [MailDestination]
  , msgSentDate    :: UTCTime
  , msgTitle       :: String
  , msgRead        :: Bool
  }
 deriving (Show)

data MailDestination = ToCorpOrAlliance CorporationID
                     | ToCharacter      CharacterID
                     | ToMailingList    ListID
 deriving (Show)

data Notification = Notification {
    ntID          :: NotificationID
  , ntType        :: NotificationType
  , ntSender      :: CharacterID
  , ntSentDate    :: UTCTime
  , ntRead        :: Bool
  }
 deriving (Show)

newtype NotificationID = NotID Integer deriving (Eq, Show)

data NotificationType = CharacterDeleted
                      | MedalReceived
                      | AllianceMaintenanceBill
                      | AllianceWarDeclared
                      | AllianceWarSurrendered
                      | AllianceWarRetracted
                      | AllianceWarInvalidated
                      | BillIssuedToChar
                      | BillIssuedToCorp
                      | BillNotPaid
                      | CharIssuedBillPaid
                      | CorpIssuedBillPaid
                      | BountyClaimed
                      | CloneActivated
                      | NewCorpMemberApplication
                      | CorpApplicationRejected
                      | CorpApplicationAccepted
                      | CorpTaxRateChanged
                      | CorpNewsReport
                      | PlayerLeftCorp
                      | NewCorpCEO
                      | CorpDividendSent
                      | CorpDividendPayout
                      | CorpVoteCreated
                      | CorpCEOVotesRevoked
                      | CorpDeclaresWar
                      | CorpWarStarted
                      | CorpSurrendersWar
                      | CorpRetractsWar
                      | CorpWarInvalidated
                      | ContainerPasswordRetrieved
                      | ContrabandOrStandingsAttack
                      | FirstShipInsurance
                      | ShipDestroyedInsurancePaid
                      | InsuranceContractInvalidated
                      | AllianceSovereigntyClaimFailed
                      | CorpSovereigntyClaimFailed
                      | AllianceSovereigntyBillLate
                      | CorpSovereigntyBillLate
                      | AllianceSovereigntyClaimLost
                      | CorpSovereigntyClaimLost
                      | AllianceSovereigntyClaimed
                      | CorpSovereigntyClaimed
                      | AllianceAnchoringAlert
                      | AllianceStructureVulnerable
                      | AllianceStructureInvulnerable
                      | SovereigntyDisruptorAnchored
                      | StructureWonOrLost
                      | CorpOfficeLeaseExpired
                      | CloneContractRevoked
                      | CorpMemberClonesMoved
                      | InsuranceContractExpired
                      | InsuranceContractIssued
                      | JumpCloneDestroyed
                      | CorporationJoiningFactionalWar
                      | CorporationLeavingFactionalWar
                      | CorporationKickedForStanding
                      | CharacterKickedForStanding
                      | CorporationWarnedForStanding
                      | CharacterWarnedForStanding
                      | CharacterLosesRank
                      | CharacterGainsRank
                      | AgentHasMoved
                      | MassTransactionReversal
                      | ReimbursementMessage
                      | AgentLocatesCharacter
                      | ResearchMissionAvailable
                      | AgentMissionOfferExpired
                      | AgentMissionTimedOut
                      | StorylineMissionOffered
                      | TutorialMessageSent
                      | TowerAlert
                      | TowerResourceAlert
                      | StationAggression
                      | StationStateChange
                      | StationConquered
                      | CorpRequestsJoinFactionalWar
                      | CorpRequestsLeaveFactionWar
                      | CorpWithdrawsJoinRequest
                      | CorpWithdrawsLeaveRequest
                      | CorporationLiquidation
                      | TerritorialClaimUnitAttacked
                      | SovereigntyBlockadeUnitAttacked
                      | InfrastructureHubAttacked
 deriving (Show)

toNotType :: Integer -> Maybe NotificationType
toNotType 02 = Just CharacterDeleted
toNotType 03 = Just MedalReceived
toNotType 04 = Just AllianceMaintenanceBill
toNotType 05 = Just AllianceWarDeclared
toNotType 06 = Just AllianceWarSurrendered
toNotType 07 = Just AllianceWarRetracted
toNotType 08 = Just AllianceWarInvalidated
toNotType 09 = Just BillIssuedToChar
toNotType 10 = Just BillIssuedToCorp
toNotType 11 = Just BillNotPaid
toNotType 12 = Just CharIssuedBillPaid
toNotType 13 = Just CorpIssuedBillPaid
toNotType 14 = Just BountyClaimed
toNotType 15 = Just CloneActivated
toNotType 16 = Just NewCorpMemberApplication
toNotType 17 = Just CorpApplicationRejected
toNotType 18 = Just CorpApplicationAccepted
toNotType 19 = Just CorpTaxRateChanged
toNotType 20 = Just CorpNewsReport
toNotType 21 = Just PlayerLeftCorp
toNotType 22 = Just NewCorpCEO
toNotType 23 = Just CorpDividendSent
toNotType 24 = Just CorpDividendPayout
toNotType 25 = Just CorpVoteCreated
toNotType 26 = Just CorpCEOVotesRevoked
toNotType 27 = Just CorpDeclaresWar
toNotType 28 = Just CorpWarStarted
toNotType 29 = Just CorpSurrendersWar
toNotType 30 = Just CorpRetractsWar
toNotType 31 = Just CorpWarInvalidated
toNotType 32 = Just ContainerPasswordRetrieved
toNotType 33 = Just ContrabandOrStandingsAttack
toNotType 34 = Just FirstShipInsurance
toNotType 35 = Just ShipDestroyedInsurancePaid
toNotType 36 = Just InsuranceContractInvalidated
toNotType 37 = Just AllianceSovereigntyClaimFailed
toNotType 38 = Just CorpSovereigntyClaimFailed
toNotType 39 = Just AllianceSovereigntyBillLate
toNotType 40 = Just CorpSovereigntyBillLate
toNotType 41 = Just AllianceSovereigntyClaimLost
toNotType 42 = Just CorpSovereigntyClaimLost
toNotType 43 = Just AllianceSovereigntyClaimed
toNotType 44 = Just CorpSovereigntyClaimed
toNotType 45 = Just AllianceAnchoringAlert
toNotType 46 = Just AllianceStructureVulnerable
toNotType 47 = Just AllianceStructureInvulnerable
toNotType 48 = Just SovereigntyDisruptorAnchored
toNotType 49 = Just StructureWonOrLost
toNotType 50 = Just CorpOfficeLeaseExpired
toNotType 51 = Just CloneContractRevoked
toNotType 52 = Just CorpMemberClonesMoved
toNotType 53 = Just CloneContractRevoked
toNotType 54 = Just InsuranceContractExpired
toNotType 55 = Just InsuranceContractIssued
toNotType 56 = Just JumpCloneDestroyed
toNotType 57 = Just JumpCloneDestroyed
toNotType 58 = Just CorporationJoiningFactionalWar
toNotType 59 = Just CorporationLeavingFactionalWar
toNotType 60 = Just CorporationKickedForStanding
toNotType 61 = Just CharacterKickedForStanding
toNotType 62 = Just CorporationWarnedForStanding
toNotType 63 = Just CharacterWarnedForStanding
toNotType 64 = Just CharacterLosesRank
toNotType 65 = Just CharacterGainsRank
toNotType 66 = Just AgentHasMoved
toNotType 67 = Just MassTransactionReversal
toNotType 68 = Just ReimbursementMessage
toNotType 69 = Just AgentLocatesCharacter
toNotType 70 = Just ResearchMissionAvailable
toNotType 71 = Just AgentMissionOfferExpired
toNotType 72 = Just AgentMissionTimedOut
toNotType 73 = Just StorylineMissionOffered
toNotType 74 = Just TutorialMessageSent
toNotType 75 = Just TowerAlert
toNotType 76 = Just TowerResourceAlert
toNotType 77 = Just StationAggression
toNotType 78 = Just StationStateChange
toNotType 79 = Just StationConquered
toNotType 80 = Just StationAggression
toNotType 81 = Just CorpRequestsJoinFactionalWar
toNotType 82 = Just CorpRequestsLeaveFactionWar
toNotType 83 = Just CorpWithdrawsJoinRequest
toNotType 84 = Just CorpWithdrawsLeaveRequest
toNotType 85 = Just CorporationLiquidation
toNotType 86 = Just TerritorialClaimUnitAttacked
toNotType 87 = Just SovereigntyBlockadeUnitAttacked
toNotType 88 = Just InfrastructureHubAttacked
toNotType _  = Nothing

-----------------------------------------------------------------------------
-- Market Orders
--

newtype OrderID = OrderID Integer deriving (Eq, Show)

data OrderState = Open
                | Closed
                | Expired
                | Fulfulled
                | Cancelled
                | Pending
                | OrderCharacterDeleted
 deriving (Eq, Show)

data Range = RangeStation
           | RangeSolarSystem
           | RangeNJumps Int
           | RangeRegion
 deriving (Eq, Show)

toOrderState :: Int -> OrderState
toOrderState 0 = Open
toOrderState 1 = Closed
toOrderState 2 = Expired -- I don't get this
toOrderState 3 = Cancelled
toOrderState 4 = Pending
toOrderState 5 = OrderCharacterDeleted
toOrderState _ = throw (EVETypeConversionError "toOrderState")

toRange :: Int -> Range 
toRange  (-1) = RangeStation
toRange     0 = RangeSolarSystem
toRange 32767 = RangeRegion
toRange     x = RangeNJumps x

data MarketOrder = MarketOrder {
    moOrderID      :: OrderID
  , moCharacterID  :: CharacterID
  , moStationID    :: StationID
  , moVolEntered   :: Integer
  , moVolRemaining :: Integer
  , moMinVolume    :: Integer
  , moOrderState   :: OrderState
  , moTypeID       :: TypeID
  , moRange        :: Range
  , moAccountKey   :: Integer
  , moDuration     :: Integer
  , moEscrow       :: Maybe Float
  , moPrice        :: Float
  , moBid          :: Bool
  , moIssued       :: UTCTime
  }
 deriving (Eq, Show)

-----------------------------------------------------------------------------
-- Medals
--

newtype MedalID = MedalID Integer deriving (Show)

data Show a => MedalAward a = MedalAward {
    medaID     :: MedalID 
  , medaReason :: String
  , medaStatus :: String
  , medaIssuer :: CharacterID
  , medaIssued :: UTCTime
  , medaExtra  :: a
  }
 deriving (Show)

data Medal = Medal {
    medID          :: MedalID
  , medTitle       :: String
  , medDescription :: String
  , medCreator     :: CharacterID
  , medCreated     :: UTCTime
  }
 deriving (Show)

-----------------------------------------------------------------------------
-- Standings
--

data StandingDir = GivenStanding | GottenStanding
 deriving (Eq, Show)

data Show a => Standing a = Standing {
    standingDirection :: StandingDir
  , standingID        :: a
  , standingName      :: String
  , standingValue     :: Float
  }
 deriving (Show)

-----------------------------------------------------------------------------
-- Wallet Stuff
--

newtype WalletDivisionID = WDID Integer deriving (Show)

data WalletJournalEntry = WalletJournalEntry {
    wjeDate        :: UTCTime
  , wjeRefID       :: RefID
  , wjeFirstParty  :: (CharacterID, String)
  , wjeSecondParty :: (CharacterID, String)
  , wjeAmount      :: Float
  , wjeBalance     :: Float
  , wjeTaxInfo     :: Maybe (CorporationID, Float)
  , wjeExtraInfo   :: TransactionInfo
  }
 deriving (Show)

data TransactionInfo = PlayerTrading StationID String
                     | MarketTransaction RefID
                     | PlayerDonation String
                     | Insurance TypeID
                     | CSPA String CharacterID
                     | CorpAccountWithdrawal String
                     | Manufacturing JobID
                     | Contract
                     | BountyPrizes SolarSystemID
                     | Unknown Integer
 deriving (Show)

data TransactionType = BUY | SELL
 deriving (Show)

instance Read TransactionType where
  readsPrec _ s = case map toLower s of
                    "buy"  -> [(BUY, "")]
                    "sell" -> [(SELL, "")]
                    _      -> []

data TransactionOwner = PERSONAL | CORPORATION
 deriving (Show)

instance Read TransactionOwner where
  readsPrec _ s = case map toLower s of
                    "personal"    -> [(PERSONAL, "")]
                    "corporation" -> [(CORPORATION, "")]
                    _             -> []

data WalletTransaction = WalletTransaction {
    wtDate           :: UTCTime
  , wtRefID          :: RefID
  , wtQuantity       :: Integer
  , wtObject         :: (TypeID, String)
  , wtPrice          :: Float
  , wtClient         :: (CharacterID, String)
  , wtStation        :: (StationID, String)
  , wtTransaction    :: TransactionType
  , wtTransactionFor :: TransactionOwner
  }
 deriving (Show)

-----------------------------------------------------------------------------
-- Container Logs
--

data ContainerLogEntry = ContainerLogEntry {
    clTime          :: UTCTime
  , clItem          :: (ItemID, TypeID)
  , clActor         :: (CharacterID, String)
  , clFlag          :: Integer -- ?
  , clLocation      :: LocationID
  , clAction        :: String
  , clPasswordType  :: String
  , clObject        :: Maybe (TypeID, Integer)
  , clConfigs       :: (String, String)
  }
 deriving (Show)

-----------------------------------------------------------------------------
-- Corporation Info
--

data Corporation = Corporation {
    corpID              :: CorporationID
  , corpName            :: String
  , corpTicker          :: String
  , corpCEO             :: (CharacterID, String)
  , corpHQ              :: (StationID, String)
  , corpDescription     :: String
  , corpURL             :: String
  , corpAlliance        :: Maybe (AllianceID, String)
  , corpTaxRate         :: Integer
  , corpMembers         :: Integer
  , corpMemberLimit     :: Maybe Integer
  , corpShared          :: Integer
  , corpDivisions       :: Maybe [(AccountID, String)]
  , corpWalletDivisions :: Maybe [(WalletDivisionID, String)]
  }
 deriving (Show)

newtype RoleID = RoleID Integer deriving (Eq, Show, Ord)

data CorpRole = CorpRole {
    roleID                 :: RoleID
  , roleName               :: String
  , roleFlags              :: [RoleFlag]
  }
 deriving (Show)

data RoleFlag = GeneralRole | GrantableRole   | AtHQ      | GrantableAtHQ
              | AtBase      | GrantableAtBase | Elsewhere | GrantableElsewhere
 deriving (Show)

newtype TitleID = TitleID Integer deriving (Eq, Show, Ord)

data CorpTitle = CorpTitle {
    titleID                :: TitleID
  , titleName              :: String
  }
 deriving (Show)

data CorpMemberSecurity = CorpMemberSecurity {
    cmsMemberID :: CharacterID
  , cmsName     :: String
  , cmsRoles    :: [CorpRole]
  , cmsTitles   :: [CorpTitle]
  }
 deriving (Show)
