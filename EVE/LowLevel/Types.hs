module EVE.LowLevel.Types
 where

import Data.Char
import Data.List
import Data.Maybe
import Data.Time.Clock
import Data.Time.Format
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
                   | EVEParseError Element
                   | UnknownError String
 deriving (Show)

type    LowLevelResult a = Either LowLevelError a
newtype CorporationID    = CorpID Integer deriving (Show,Eq)
newtype FactionID        = FacID  Integer deriving (Show,Ord,Eq)
newtype StationID        = StatID Integer deriving (Show,Ord,Eq)
newtype SolarSystemID    = SSID   Integer deriving (Show,Ord,Eq)
newtype LocationID       = LocID  Integer deriving (Show,Ord,Eq)
newtype RefID            = RID    Integer deriving (Show)
newtype ListID           = ListID Integer deriving (Show)
newtype MessageID        = MsgID  Integer deriving (Show)

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

data Attribute = Intelligence
               | Perception
               | Charisma
               | Willpower
               | Memory
 deriving (Show)

instance Read Attribute where
  readsPrec _ s = case map toLower s of
                    "intelligence" -> [(Intelligence,"")]
                    "perception"   -> [(Perception,"")]
                    "charisma"     -> [(Charisma,"")]
                    "willpower"    -> [(Willpower,"")]
                    "memory"       -> [(Memory,"")]
                    _              -> []

data SkillLevel = SkillLevel {
                    levelSkill :: SkillID
                  , levelLevel :: Integer
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
           | ConnectionMutator Double
           | ConsumptionQuantity Integer
           | ConsumptionQuantityPerc Integer
           | CopySpeed Integer
           | CorporationMember Integer
           | CPUNeed Integer
           | CPUOutputBonus Integer
           | CriminalConnectionsMutator Double
           | DamageCloudChanceReduction Integer
           | DamageHP Integer
           | DamageMultiplier Integer
           | DiplomacyMutator Double
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
           | Uniformity Double
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

pb :: String -> String -> Bonus
pb "accessDifficultyBonus"               v = AccessDifficulty           $ read v
pb "agilityBonus"                        v = Agility                    $ read v
pb "aoeCloudSizeBonus"                   v = AreaOfEffectSize           $ read v
pb "aoeVelocityBonus"                    v = AreaOfEffectVelocity       $ read v
pb "armorHpBonus"                        v = ArmorHitPoints             $ read v
pb "blueprintmanufactureTimeBonus"       v = BlueprintManufactureTime   $ read v
pb "boosterAttributeModifier"            v = BoosterAttributeModifier   $ read v
pb "boosterChanceBonus"                  v = BoosterChance              $ read v
pb "bountySkillBonus"                    v = BountySkill                $ read v
pb "canNotBeTrainedOnTrial"              _ = CanNotBeTrainedOnTrial
pb "capNeedBonus"                        v = CapNeed                    $ read v
pb "capRechargeBonus"                    v = CapRecharge                $ read v
pb "capacitorCapacityBonus"              v = CapacitorCapacity          $ read v
pb "capacitorNeedMultipler"              v = CapacitorNeedMult          $ read v
pb "charismaBonus"                       _ = CharismaBonus
pb "cloakingTargetingDelayBonus"         v = CloakingTargetingDelay     $ read v
pb "connectionBonusMutator"              v = ConnectionMutator          $ read v
pb "consumptionQuantityBonus"            v = ConsumptionQuantity        $ read v
pb "consumptionQuantityBonusPercentage"  v = ConsumptionQuantityPerc    $ read v
pb "copySpeedBonus"                      v = CopySpeed                  $ read v
pb "corporationMemberBonus"              v = CorporationMember          $ read v
pb "cpuNeedBonus"                        v = CPUNeed                    $ read v
pb "cpuOutputBonus2"                     v = CPUOutputBonus             $ read v
pb "criminalConnectionsMutator"          v = CriminalConnectionsMutator $ read v
pb "damageCloudChanceReduction"          v = DamageCloudChanceReduction $ read v
pb "damageHP"                            v = DamageHP                   $ read v
pb "damageMultiplierBonus"               v = DamageMultiplier           $ read v
pb "diplomacyMutator"                    v = DiplomacyMutator           $ read v
pb "droneMaxVelocityBonus"               v = DroneMaxVelocity           $ read v
pb "droneRangeBonus"                     v = DroneRange                 $ read v
pb "durationBonus"                       v = Duration                   $ read v
pb "durationSkillBonus"                  v = DurationSkill              $ read v
pb "falloffBonus"                        v = FalloffBonus               $ read v
pb "fastTalkMutator"                     v = FastTalkMutator            $ read v
pb "hardeningBonus"                      v = Hardening                  $ read v
pb "hardeningbonus2"                     v = Hardening2                 $ read v
pb "hullHpBonus"                         v = HullHP                     $ read v
pb "iceHarvestCycleBonus"                v = IceHarvestCycle            $ read v
pb "intelligenceBonus"                   _ = IntelligenceBonus
pb "inventionBonus"                      v = Invention                  $ read v
pb "jumpDriveCapacitorNeedBonus"         v = JumpDriveCapacitorNeed     $ read v
pb "jumpDriveRangeBonus"                 v = JumpDriveRange             $ read v
pb "laboratorySlotsBonus"                v = LaboratorySlots            $ read v
pb "learningBonus"                       v = Learning                   $ read v
pb "manufactureCostBonus"                v = ManufactureCost            $ read v
pb "manufacturingSlotBonus"              v = ManufactureSlot            $ read v
pb "manufacturingTimeBonus"              v = ManufactureTime            $ read v
pb "maxActiveDroneBonus"                 v = MaxActiveDrones            $ read v
pb "maxAttackTargets"                    v = MaxAttackTargets           $ read v
pb "maxFlightTimeBonus"                  v = MaxFlightTime              $ read v
pb "maxJumpClones"                       v = MaxJumpClones              $ read v
pb "maxJumpClonesBonus"                  v = MaxJumpClones2             $ read v
pb "maxScanDeviationModifier"            v = MaxScanDeviationModifier   $ read v
pb "maxTargetBonus"                      v = MaxTargetBonus             $ read v
pb "maxTargetRangeBonus"                 v = MaxTargetRange             $ read v
pb "memoryBonus"                         _ = MemoryBonus
pb "mineralNeedResearchBonus"            v = MineralNeedResearch        $ read v
pb "miningAmountBonus"                   v = MiningAmount               $ read v
pb "miningUpgradeCPUReductionBonus"      v = MiningUpgradeCPUReduction  $ read v
pb "minmatarTechMutator"                 v = MinmatarTechMutator        $ read v
pb "missileVelocityBonus"                v = MissileVelocity            $ read v
pb "moduleRepairRateBonus"               v = ModuleRepairRate           $ read v
pb "negotiationBonus"                    v = Negotiation                $ read v
pb "nonRaceCorporationMembersBonus"      v = NonRaceCorporationMembers  $ read v
pb "perceptionBonus"                     _ = PerceptionBonus
pb "posStructureControlAmount"           v = POSStructureControl        $ read v
pb "powerEngineeringOutputBonus"         v = PowerEngineeringOutput     $ read v
pb "powerNeedBonus"                      v = PowerNeed                  $ read v
pb "projECMDurationBonus"                v = ProjECMDuration            $ read v
pb "rangeSkillBonus"                     v = Range                      $ read v
pb "rechargeratebonus"                   v = RechargeRate               $ read v
pb "refiningYieldMutator"                v = RefiningYieldMutator       $ read v
pb "researchGangSizeBonus"               v = ResearchGangSize           $ read v
pb "resistanceBonus"                     v = Resistance                 $ read v
pb "rigDrawbackBonus"                    v = RigDrawback                $ read v
pb "rofBonus"                            v = RateOfFire                 $ read v
pb "scanResolutionBonus"                 v = ScanResolution             $ read v
pb "scanSkillEwStrengthBonus"            v = ScanEWStrength             $ read v
pb "scanSkillTargetPaintStrengthBonus"   v = TargetPaintStrength        $ read v
pb "scanStrengthBonus"                   v = ScanStrength               $ read v
pb "scanspeedBonus"                      v = ScanSpeed                  $ read v
pb "shieldBoostCapacitorBonus"           v = ShieldBoostCapacitor       $ read v
pb "shieldCapacityBonus"                 v = ShieldCapacity             $ read v
pb "shieldRechargerateBonus"             v = ShieldRecharge             $ read v
pb "shipBrokenRepairCostMultiplierBonus" v = ShipBrokenRepCostMult      $ read v
pb "shipPowerBonus"                      v = ShipPower                  $ read v
pb "socialBonus"                         v = Social                     $ read v
pb "socialMutator"                       v = SocialMutator              $ read v
pb "speedFBonus"                         v = SpeedF                     $ read v
pb "speedFactor"                         v = SpeedFactor                $ read v
pb "squadronCommandBonus"                v = SquadronCommand            $ read v
pb "thermodynamicsHeatDamage"            v = ThermodynamicsHeatDamage   $ read v
pb "trackingSpeedBonus"                  v = TrackingSpeed              $ read v
pb "tradePremiumBonus"                   v = TradePremium               $ read v
pb "turretSpeeBonus"                     v = TurretSpeed                $ read v
pb "uniformityBonus"                     v = Uniformity                 $ read v
pb "velocityBonus"                       v = Velocity                   $ read v
pb "warpCapacitorNeedBonus"              v = WarpCapacitorNeed          $ read v
pb "willpowerBonus"                      _ = WillpowerBonus
pb ub                                   uv = UnknownBonus ub uv

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
    facID                :: FactionID
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
    allID         :: AllianceID
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
  | SkillInTraining
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
toLocFlag  61 = Just SkillInTraining
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
fromLocFlag SkillInTraining             = 61
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

newtype CharacterID      = CharID Integer deriving (Show,Eq)
data Gender = Male | Female               deriving (Show,Eq)

instance Read Gender where
  readsPrec d x = case map toLower x of
                    s | "male"   `isPrefixOf` s -> [(Male,   drop 4 x)]
                      | "female" `isPrefixOf` s -> [(Female, drop 6 x)]
                      | otherwise               -> []

data Character = Character {
    charID                 :: CharacterID
  , charName               :: String
  , charRace               :: String
  , charBloodline          :: String
  , charGender             :: Gender
  , charCorporationName    :: String
  , charCorporationID      :: CorporationID
  , charBalance            :: Double
  , charAttributeEnhancers :: [AttributeEnhancer]
  , charIntelligence       :: Int
  , charMemory             :: Int
  , charCharisma           :: Int
  , charPerception         :: Int
  , charWillpower          :: Int
  , charSkills             :: [(SkillLevel, Integer)]
  }
 deriving (Show)

data AttributeEnhancer = AttrEnh {
    attrenAttribute :: Attribute
  , attrenName      :: String
  , attrenBonus     :: Integer
  }
 deriving (Show)

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
-- Mail Messages
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
                | CharacterDeleted

data Range = Station
           | SolarSystem
           | NJumps Int
           | Region

toOrderState :: Int -> OrderState
toOrderState 0 = Open
toOrderState 1 = Closed
toOrderState 2 = Expired -- I don't get this
toOrderState 3 = Cancelled
toOrderState 4 = Pending
toOrderState 5 = CharacterDeleted

toRange :: Int -> Range 
