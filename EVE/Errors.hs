{-# LANGUAGE DeriveDataTypeable #-}
module EVE.Errors where

import Control.Exception
import Data.Typeable
import Text.XML.Light

-- |The set of exceptions that can be raised within the EVE monad
data EVEError = MustSetCharacterFirst -- ^You must set a character before
                                      -- performing this call
              | UnknownCharacter -- ^The given character was not associated
                                 -- with the provided key.
  deriving (Show, Typeable)

instance Exception EVEError

-- |Low-level errors that can occur when utilizing the online API services
data EVELowLevelError = ConnectionReset
                      | ConnectionClosed
                      | HTTPParseError String
                      | XMLParseError String
                      | EVEParseError Element
                      | EVETypeConversionError String
                      | UnknownError String
 deriving (Show, Typeable)

instance Exception EVELowLevelError


