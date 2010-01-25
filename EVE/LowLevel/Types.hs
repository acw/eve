module EVE.LowLevel.Types
 where

import Data.Char
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
newtype CharacterID      = CID String
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

type SkillBonus = (String, String)

data Skill = Skill {
               skillName         :: String
             , skillGroup        :: Integer
             , skillID           :: Integer
             , skillDescription  :: String
             , skillRank         :: Integer
             , skillPrimary      :: Attribute
             , skillSecondary    :: Attribute
             , skillRequirements :: [SkillLevel]
             , skillBonuses      :: [SkillBonus]
             }
 deriving (Show)

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


