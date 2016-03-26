{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
module Dotlan where

import Control.Monad(foldM)
import Data.Char(isSpace)
import Data.Maybe(mapMaybe)
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as M
import Data.Text(pack)
import Data.Yaml
import System.IO
import Text.HTML.Scalpel
import Text.Read(readMaybe)

baseScraper :: Scraper String [[[String]]]
baseScraper =
  chroots (("table" :: String) @: [hasClass "tablelist"]) $
    chroots ("tr" :: String) $
      texts ("td"  :: String)

processBaseScrape :: [String] -> Maybe (String, (Double, Word, Word, Word))
processBaseScrape [c,ss,sec,cl,img,emp,_,st,de,la,pl,ri,mo] =
  processBaseScrape [c,ss,sec,cl,img,emp,st,de,la,pl,ri,mo]
processBaseScrape [_constellation, solarSystem, security, _class, _img,
                   _empire, _stations, _ded, _landmarks, _planets,
                   rockIce, moons]
  | Just security' <- readMaybe security, Just moons' <- readMaybe moons =
      let solarSystem' = filter (not . isSpace) solarSystem
          (belts, plusIce) = break (== '+') rockIce
          ice = case plusIce of
                  ('+':rest) | Just amt <- readMaybe rest -> amt
                  _                                       -> 0
      in case readMaybe belts of
           Nothing -> Nothing
           Just belts' ->
             Just (solarSystem', (security', belts', ice, moons'))
processBaseScrape _ = Nothing

type PlanetInfo = (Word, Word, Word, Word, Word, Word, Word, Word, Word)


processPlanetScrape :: [String] -> Maybe (String, PlanetInfo)
processPlanetScrape [_constellation, solarSystem, temp, ice, gas,
                      ocean, lava, barren, storm, plasma, shattered]
  | temp'      <- readPlanetCount temp,
    ice'       <- readPlanetCount ice,
    gas'       <- readPlanetCount gas,
    ocean'     <- readPlanetCount ocean,
    lava'      <- readPlanetCount lava,
    barren'    <- readPlanetCount barren,
    storm'     <- readPlanetCount storm,
    plasma'    <- readPlanetCount plasma,
    shattered' <- readPlanetCount shattered =
      Just (filter (not . isSpace) solarSystem,
            (temp', ice', gas', ocean', lava', barren', storm', plasma', shattered'))
processPlanetScrape _ = Nothing

readPlanetCount :: String -> Word
readPlanetCount x
  | Just v <- readMaybe x = v
  | otherwise             = 0

data SystemInfo = SI String Double Word Word Word PlanetInfo
  deriving (Show)

instance ToJSON SystemInfo where
  toJSON (SI _ sec belts ice moons (t, i, g, o, l, b, s, p, h)) =
    object [ "security" .= sec
           , "belts"    .= belts
           , "ice"      .= ice
           , "moons"    .= moons
           , "planets"  .=
                object [ "temperate" .= t
                       , "ice"       .= i
                       , "gas"       .= g
                       , "ocean"     .= o
                       , "lava"      .= l
                       , "barren"    .= b
                       , "storm"     .= s
                       , "plasma"    .= p
                       , "shattered" .= h ] ]

newtype SystemMap = SM (Map String SystemInfo)
  deriving (Show)

instance ToJSON SystemMap where
  toJSON (SM smap) = object (map (\ (n, v) -> pack n .= v) (M.toList smap))

instance Monoid SystemMap where
  mempty                = SM M.empty
  mappend (SM a) (SM b) = SM (M.union a b)

getSystemMap :: String -> IO SystemMap
getSystemMap region =
  do putStr ("Fetching " ++ region ++ " ... ")
     hFlush stdout
     let mainUrl   = "http://evemaps.dotlan.net/region/" ++ region
         planetUrl = mainUrl ++ "/planets"
     Just resBase  <- scrapeURL mainUrl   baseScraper
     Just planBase <- scrapeURL planetUrl baseScraper
     let map1 = M.fromList (concat (map (mapMaybe processBaseScrape) resBase))
         map2 = M.fromList (concat (map (mapMaybe processPlanetScrape) planBase))
         map3 = M.intersectionWithKey (\ n (a, b, c, d) e -> SI n a b c d e) map1 map2
     putStrLn ("done (" ++ show (M.size map3) ++ " systems)")
     return (SM map3)

regions :: [String]
regions =
  ["Aridia", "Black Rise", "The Bleak Lands", "The Citadel", "Derelik", "Devoid",
   "Domain", "Essence", "Everyshore", "The Forge", "Genesis", "Heimatar", "Kador",
   "Khanid", "Kor-Azor", "Lonetrek", "Metropolis", "Molden_Heath", "Placid", "Sinq Laison",
   "Solitude", "Tash-Murkon", "Verge Vendor", "Branch", "Cache", "Catch", "Cloud Ring",
   "Cobalt Edge", "Curse", "Deklein", "Delve", "Detorid", "Esoteria", "Etherium Reach",
   "Fade", "Feythabolis", "Fountain", "Geminate", "Great Wildlands", "Immensea",
   "Impass", "Insmother", "The Kalevala Expanse", "Malpais", "Oasa", "Omist",
   "Outer Passage", "Outer Ring", "Paragon Soul", "Period Basis", "Perrigen Falls",
   "Providence", "Pure Blind", "Querious", "Scalding Pass", "The Spire", "Stain",
   "Syndicate", "Tenal", "Tenerifis", "Tribute", "Vale of the Silent", "Venal",
   "Wicked Creek"]

pull :: IO ()
pull =
  do res <- foldM (\ acc n -> (acc `mappend`) `fmap` getSystemMap n) mempty regions
     encodeFile "planetInfo.yaml" res
