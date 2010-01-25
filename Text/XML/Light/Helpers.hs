module Text.XML.Light.Helpers
 where

import Control.Monad
import Data.Maybe
import Text.XML.Light

mapChildren :: String -> Element -> (Element -> Maybe a) -> Maybe [a]
mapChildren s e f = sequence $ map f $ findChildren (unqual s) e

foldChildren :: String -> Element -> a -> (a -> Element -> Maybe a) -> Maybe a
foldChildren s e b f = foldM f b $ findChildren (unqual s) e

mapElements :: String -> Element -> (Element -> Maybe a) -> Maybe [a]
mapElements s e f = sequence $ map f $ findElements (unqual s) e

foldElements :: String -> Element -> a -> (a -> Element -> Maybe a) -> Maybe a
foldElements s e b f = foldM f b $ findElements (unqual s) e

--

mapChildrenWithAttName :: String -> Element -> (Element -> Maybe a) -> Maybe [a]
mapChildrenWithAttName s e f = mapM f $ findChildrenWithAttName s e

mapElementsWithAttName :: String -> Element -> (Element -> Maybe a) -> Maybe [a]
mapElementsWithAttName s e f = mapM f $ findElementsWithAttName s e

foldChildrenWithAttName :: String -> Element -> a ->
                           (a -> Element -> Maybe a) ->
                           Maybe a
foldChildrenWithAttName s e b f = foldM f b $ findChildrenWithAttName s e


foldElementsWithAttName :: String -> Element -> a ->
                           (a -> Element -> Maybe a) ->
                           Maybe a
foldElementsWithAttName s e b f = foldM f b $ findElementsWithAttName s e


--

getChildData :: String -> Element -> Maybe String
getChildData s x = strContent `fmap` findChild (unqual s) x

getElementData :: String -> Element -> Maybe String
getElementData s x = strContent `fmap` findElement (unqual s) x

--

findChildWithAttName :: String -> Element -> Maybe Element
findChildWithAttName s = listToMaybe . findChildrenWithAttName s

findChildrenWithAttName :: String -> Element -> [Element]
findChildrenWithAttName s = filterChildren (elementHasNameAttr s)

findElementWithAttName :: String -> Element -> Maybe Element
findElementWithAttName  s = listToMaybe . findElementsWithAttName s

findElementsWithAttName :: String -> Element -> [Element]
findElementsWithAttName s = filterElements (elementHasNameAttr s)

elementHasNameAttr :: String -> Element -> Bool
elementHasNameAttr s e =
  case findAttr (unqual "name") e of
    Nothing -> False
    Just v  -> s == v
