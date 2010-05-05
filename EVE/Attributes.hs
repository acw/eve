module EVE.Attributes(
         Attribute(..)
       , AttributeSet(..), BaseAttributeSet, EffectiveAttributeSet
       , skillPointsPerHour
       )
 where

import Data.Char(toLower)

-- |An attribute name
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

-- |A set of attributes
data Num a => AttributeSet a = AttributeSet {
    asIntelligence :: a
  , asPerception   :: a
  , asCharisma     :: a
  , asWillpower    :: a
  , asMemory       :: a
  }
 deriving (Show)

-- |Base attributes for a character
type BaseAttributeSet = AttributeSet Integer

-- |Effective attributes for a character, taking into account learning skills
-- and implants.
type EffectiveAttributeSet = AttributeSet Float

-- |Given the primary attribute, secondary attribute, and an attribute set,
-- computes the number of skill points you will get per hour.
skillPointsPerHour :: Attribute -> Attribute -> EffectiveAttributeSet -> Float
skillPointsPerHour primary secondary set =
  (lookupAttInSet primary set) + (lookupAttInSet secondary set / 2)

-- |Given an attribute, look up its value in the attribute set.
lookupAttInSet :: Num a => Attribute -> AttributeSet a -> a
lookupAttInSet Intelligence = asIntelligence
lookupAttInSet Perception   = asPerception
lookupAttInSet Charisma     = asCharisma
lookupAttInSet Willpower    = asWillpower
lookupAttInSet Memory       = asMemory

-- |Compute an effective attribute based on the base attribute, the attribute-
-- specific learning skill levels, any attribute bonuses, and the general
-- learning skill level.
effectiveAttribute :: Integer -> Integer -> Integer -> Integer -> Float
effectiveAttribute base aslearn implant learn = 
  (base' + aslearn' + implant') * (1 + (0.02 * learn'))
 where
  base'    = fromInteger base
  aslearn' = fromInteger aslearn
  implant' = fromInteger implant
  learn'   = fromInteger learn
