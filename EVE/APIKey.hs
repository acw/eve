module EVE.APIKey(
         APIKey(..)
       , LimitedAPIKey, mkLimitedAPIKey
       , FullAPIKey, mkFullAPIKey
       )
 where

-- * API Key types, class, and functions

data LimitedAPIKey = LimitedAPIKey { lkeyUser :: String, lkeyKey :: String }
data FullAPIKey    = FullAPIKey    { fkeyUser :: String, fkeyKey :: String }

class APIKey t where
  getUser :: t -> String
  getKey  :: t -> String

instance APIKey LimitedAPIKey where
  getUser = lkeyUser
  getKey  = lkeyKey

instance APIKey FullAPIKey where
  getUser = fkeyUser
  getKey  = fkeyKey

-- |Make a limited API key from the given user ID and API key. You can get
-- this information from this web page: <https://www.eveonline.com/login.asp>
mkLimitedAPIKey :: String -> String -> LimitedAPIKey
mkLimitedAPIKey = LimitedAPIKey

-- |Make a full API key from the given user ID and API key. You can get
-- this information from this web page: <https://www.eveonline.com/login.asp>
mkFullAPIKey :: String -> String -> FullAPIKey
mkFullAPIKey = FullAPIKey


