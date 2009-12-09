module Properties where

data Property
  = Volume Int
  | Unknown String String

readMaybe :: Read a => String -> Maybe a
readMaybe xs = case reads xs of
  [(a,"") -> Just a
  _ -> Nothing

parseProperty :: String -> String -> Maybe Property
parseProperty key value = case key of
  "volume" -> f Volume
  _ -> Just (Unknown key value)

  where
  f con = con `fmap` readMaybe value
