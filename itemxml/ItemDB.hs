module ItemDB where

import Text.XML.Light
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Ord
import Data.List
import Control.Monad
import Control.Arrow (first)
import MonadLib

data ItemDB = ItemDB [ItemCategory]
 deriving Show

data ItemCategory = ItemCategory
  { categoryId :: Int
  , categoryName, categoryIcon, categoryDescription :: String
  , categoryData :: Either [ItemCategory] [Item] }
 deriving Show

data Item = Item
  { itemId :: Int
  , itemIcon, itemName :: String
  , itemDescription :: Maybe String
  , itemProperties :: Map String String
  }
 deriving Show

findItem name = find f . flattenItemDB
  where
  f (_, i) = itemName i == name

itemDifferences a b = difference (Map.toList (itemProperties a))
                                 (Map.toList (itemProperties b))
  where
  difference [] [] = []
  difference [] ( (k,v) : xs) = (k, "", v) : difference [] xs
  difference ( (k,v) : xs) [] = (k, v, "") : difference xs []
  difference xxs@( (k1,v1) : xs) yys@( (k2,v2) : ys )
    = case compare k1 k2 of
        LT -> (k1, v1, "") : difference xs yys
        GT -> (k2, "", v2) : difference xxs ys
        EQ | v1 == v2 -> difference xs ys
           | otherwise -> (k1, v1, v2) : difference xs ys



flattenItemDB :: ItemDB -> [([String],Item)]
flattenItemDB (ItemDB categories) = flattenCategory =<< categories
  where
  flattenCategory c = map (first (categoryName c :))
                    $ case categoryData c of
                        Left cs -> flattenCategory =<< cs
                        Right is -> map flattenItem is
  flattenItem i = ([], i)

type M = ExceptionT String Id

parseItemXml :: String -> Either String ItemDB
parseItemXml c = runId $ runExceptionT $ case parseXMLDoc c of
  Nothing -> raise "Unable to parse XML"
  Just e -> elementToItemDB e

getAttr :: String -> Element -> M String
getAttr name e
  = case findAttr (unqual name) e of
      Nothing -> raise ("Could not find attribute: " ++ show name)
      Just  v -> return v

getChild :: String -> Element -> M Element
getChild name e
  = case findChild (unqual name) e of
      Nothing -> raise ("Could not find child: " ++ show name ++ " in element " ++ show (elName e))
      Just  v -> return v

readM :: Read a => String -> M a
readM xs = case reads xs of
             [(a, "")] -> return a
             _ -> raise $ "readM: Bad parse: " ++ show xs

elementToItemDB :: Element -> M ItemDB
elementToItemDB e = do
  versionString <- getAttr "version" e
  unless (versionString == "1")
    (raise "ItemDB version 1 expected")

  categories <- getCategoryBody e
  return (ItemDB categories)

getCategoryBody = mapM elementToCategory
                . findChildren (unqual "ItemCategory")

elementToCategory :: Element -> M ItemCategory
elementToCategory e = do
  iD   <- readM =<< getAttr "id" e
  name <- getAttr "name" e
  icon <- getAttr "icon" e
  desc <- strContent `fmap` getChild "Description" e

  let mkCategory dat = ItemCategory
                { categoryId = iD
                , categoryName = name
                , categoryIcon = icon
                , categoryDescription = desc
                , categoryData = dat
                }

  case (findChild (unqual "SubCategories") e, findChild (unqual "ItemList") e)of
    (Just sub, Nothing  ) -> fmap (mkCategory . Left) (getCategoryBody sub)
    (Nothing , Just list) -> fmap (mkCategory . Right) (getItems list)
    (Nothing, Nothing) -> return (mkCategory (Right []))
    _ -> raise ("Malformed category: " ++ show name)

getItems :: Element -> M [Item]
getItems = mapM getItem . findChildren (unqual "Item")

getItem :: Element -> M Item
getItem e = do
  iD <- readM =<< getAttr "id" e
  icon <- getAttr "icon" e
  name <- getAttr "name" e
  let desc = strContent `fmap` findChild (unqual "Description") e
  prop <- getProperties =<< getChild "Properties" e
  return Item
    { itemName = name
    , itemDescription = desc
    , itemIcon = icon
    , itemId = iD
    , itemProperties = prop
    }

getProperties :: Element -> M (Map String String)
getProperties e = do
  xs <- mapM getProperty (findChildren (unqual "Property") e)
  return (Map.fromList xs)

getProperty :: Element -> M (String,String)
getProperty e = do
  name <- getAttr "name" e
  val  <- readM (strContent e)
  return (name, val)
