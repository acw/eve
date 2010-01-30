module EVE.LowLevel.Types
 where

import Data.Char
import Data.List
import Data.Maybe
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
newtype CharacterID      = CharID Integer deriving (Show)
newtype CorporationID    = CorpID Integer deriving (Show)
newtype FactionID        = FacID Integer  deriving (Show)
newtype ItemID           = IID String
newtype RefID            = RID String

-----------------------------------------------------------------------------
-- Skills and such
--

data SkillGroup = SkillGroup {
                    groupName :: String
                  , groupID   :: Integer
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
                    levelSkill :: Integer
                  , levelLevel :: Integer
                  }
 deriving (Show)

data Skill = Skill {
               skillName         :: String
             , skillGroup        :: Integer
             , skillID           :: Integer
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
    return $ SkillLevel (read skill) (read level)

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
-- Map data
--

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
