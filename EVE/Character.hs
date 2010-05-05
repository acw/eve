module EVE.Character(
         getCharacters
       , selectCharacter
       , selectCharacterName
       , characterBalance
       , characterSheet
       )
 where

import Control.Applicative
import Data.Char(toLower)
import Data.List(isPrefixOf)
import Data.Maybe(catMaybes)
import EVE.Attributes
import EVE.Corporation
import EVE.Errors
import EVE.Monad.Internal
import EVE.Skills
import Text.XML.Light
import Text.XML.Light.Helpers

-- |Gender information. Blame EVE for its binary gender forms, if you're
-- inclined to argue.
data Gender = Male | Female               deriving (Show,Eq)

instance Read Gender where
  readsPrec _ x = case map toLower x of
                    s | "male"   `isPrefixOf` s -> [(Male,   drop 4 x)]
                      | "female" `isPrefixOf` s -> [(Female, drop 6 x)]
                      | otherwise               -> []

-- |The four character races in EVE.
data Race = Minmatar
          | Gallente
          | Amarr
          | Caldari
 deriving (Show, Eq)

instance Read Race where
  readsPrec _ x = case map toLower x of
                    "minmatar" -> [(Minmatar, "")]
                    "gallente" -> [(Gallente, "")]
                    "amarr"    -> [(Amarr,    "")]
                    "caldari"  -> [(Caldari,  "")]
                    _          -> []

-- |Information about a character.
data Character = Character {
    -- |The character ID of the character.
    charID                 :: CharacterID
    -- |The character's in-game name.
  , charName               :: String
    -- |The character's race.
  , charRace               :: Race
    -- |The character's bloodline.
  , charBloodline          :: String
    -- |The character's gender.
  , charGender             :: Gender
    -- |The corporation info for the corporation the character is
    -- associated with.
  , charCorporation        :: (String, CorporationID)
    -- |The character's account balance.
  , charBalance            :: Double
    -- |The character's implants that affect their attributes.
  , charAttributeEnhancers :: [AttributeEnhancer]
    -- |The character's attributes.
  , charAttributes         :: BaseAttributeSet
    -- |The character's skills. Listed as a tuple of current skill level and
    -- skill points earned.
  , charSkills             :: [(SkillLevel, Integer)]
    -- |The character's certificates.
  , charCertificates       :: [CertificateID]
  }
 deriving (Show)

-- |An implant that effects an attribute.
data AttributeEnhancer = AttrEnh {
    -- |The attribute modified
    attrenAttribute :: Attribute
    -- |The name of the implant
  , attrenName      :: String
    -- |The bonus provided by the implant
  , attrenBonus     :: Integer
  }
 deriving (Show)

-- |A skill level.
data SkillLevel = SkillLevel {
    levelSkill :: SkillID
  , levelLevel :: Integer
  }
 deriving (Show)

-- |Returns a list of the characters associated with the given API key. Based
-- on this list, you should select a character using 'selectCharacter', below.
getCharacters :: APIKey k => EVE k [(String, CharacterID)]
getCharacters  = do
  key <- getKey
  runRequest "account/Characters" (keyToArgs key) $ parseRows $ \ r -> do
    name <-                       findAttr (unqual "name")            r
    char <- CharID <$> (mread =<< findAttr (unqual "characterID")     r)
    return (name, char)

-- |Select the given character for all future actions.
selectCharacter :: APIKey k => CharacterID -> EVE k ()
selectCharacter c = setCharacter c

-- |Select the given character name. If the given name is not associated with
-- the key associate with the current EVE run, then an exception
-- ('UnknownCharacter') will be raised.
selectCharacterName :: APIKey k => String -> EVE k ()
selectCharacterName n = getCharacters >>= \ l ->
  case lookup n l of
    Just ident -> selectCharacter ident
    Nothing    -> throwEVE UnknownCharacter

-- |Get the current character's balance.
characterBalance :: EVE FullAPIKey Float
characterBalance = standardRequest "char/AccountBalance" $ \ x -> do
  mread =<< findAttr (unqual "balance") =<< findElement (unqual "row") x

-- |Get the current character's character sheet.
characterSheet :: APIKey k => EVE k Character
characterSheet = standardRequest "char/CharacterSheet" $ \ x -> do
  cid <- mread =<< getElementData "characterID"     x
  nm  <-           getElementData "name"            x
  rc  <- mread =<< getElementData "race"            x
  bl  <-           getElementData "bloodLine"       x
  gn  <- mread =<< getElementData "gender"          x
  cn  <-           getElementData "corporationName" x
  ci  <- mread =<< getElementData "corporationID"   x
  bal <- mread =<< getElementData "balance"         x
  int <- mread =<< getElementData "intelligence"    x
  mem <- mread =<< getElementData "memory"          x
  cha <- mread =<< getElementData "charisma"        x
  per <- mread =<< getElementData "perception"      x
  wil <- mread =<< getElementData "willpower"       x
  let enhs = catMaybes [getEnhancer "intelligenceBonus" Intelligence x
                       ,getEnhancer "memoryBonus"       Memory       x
                       ,getEnhancer "charismaBonus"     Charisma     x
                       ,getEnhancer "perceptionBonus"   Perception   x
                       ,getEnhancer "willpowerBonus"    Willpower    x]
      atts = AttributeSet int mem cha per wil
  sks <- do base <- findElementWithAttName "skills" x
            mapChildren "row" base $ \ sk -> do
              typ <- mread =<< findAttr (unqual "typeID")      sk
              lev <- mread =<< findAttr (unqual "level")       sk
              pnt <- mread =<< findAttr (unqual "skillpoints") sk
              return (SkillLevel typ lev, pnt)
  cts <- do base <- findElementWithAttName "certificates" x
            mapChildren "row" base $ \ ct ->
              mread =<< findAttr (unqual "certificateID") ct
  return (Character cid nm rc bl gn (cn, ci) bal enhs atts sks cts)
 where
  getEnhancer :: String -> Attribute -> Element -> Maybe AttributeEnhancer
  getEnhancer s attr xml = do
    el <- findElement (unqual s) xml
    nm <-           getChildData "augmentatorName" el
    vl <- mread =<< getChildData "augmentatorValue" el
    return (AttrEnh attr nm vl)



