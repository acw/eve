module EVE.Corporation(
         CorporationID
       )
 where

import Control.Arrow(first)

newtype CorporationID = CorpID Integer

instance Read CorporationID where
  readsPrec p s = map (first CorpID) (readsPrec p s)

instance Show CorporationID where
  show (CorpID x) = "corp:" ++ show x
