{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
module Control.Lens.Criteria(
         Criteria(..)
       , criteriaHolds
       )
 where

import Control.Lens
import Data.Bits(xor)

data Criteria a =           All
                |           None
                | forall b. Test (b -> Bool)  (Getter a b)
                |           Or   (Criteria a) (Criteria a)
                |           And  (Criteria a) (Criteria a)
                |           Xor  (Criteria a) (Criteria a)
                |           Not  (Criteria a)

criteriaHolds :: Criteria a -> a -> Bool
criteriaHolds All               _ = True
criteriaHolds None              _ = False
criteriaHolds (Or a b)          x = criteriaHolds a x || criteriaHolds b x
criteriaHolds (And a b)         x = criteriaHolds a x && criteriaHolds b x
criteriaHolds (Xor a b)         x = criteriaHolds a x `xor` criteriaHolds b x
criteriaHolds (Not a)           x = not (criteriaHolds a x)
criteriaHolds (Test test field) x = test (view field x)
