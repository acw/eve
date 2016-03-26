module FactionConvert where

import qualified Data.HashMap.Strict as HM
import Data.List(partition)
import Data.Scientific(fromFloatDigits)
import Data.Text(Text,pack)
import Data.Yaml hiding (Value,Null)
import qualified Data.Yaml as Y
import Database.SQLite

processRows :: Row Value -> HM.HashMap Text Y.Value -> HM.HashMap Text Y.Value
processRows row acc =
  case partition (\ (a, b) -> a == "factionID") row of
    ([("factionID", Int fid)], rest) ->
      HM.insert (text fid) (Object (convertFields rest)) acc
    _ ->
      error ("Couldn't find factionID?")

text :: Show a => a -> Text
text = pack . show

convertFields :: Row Value -> HM.HashMap Text Y.Value
convertFields [] = HM.empty
convertFields ((k,v):rest) =
  let rest' = convertFields rest
      k'    = pack k
  in case v of
       Double d -> HM.insert k' (Number (fromFloatDigits d)) rest'
       Int i    -> HM.insert k' (Number (fromIntegral i)) rest'
       Text s   -> HM.insert k' (String (pack s)) rest'
       Blob b   -> error "Blob encountered"
       Null     -> rest'

run :: IO ()
run =
  do sql <- openReadonlyConnection "/Users/awick/Downloads/sqlite-latest.sqlite"
     Right rows <- execStatement sql "SELECT * FROM chrFactions"
     let res = foldr processRows HM.empty (concat rows)
     encodeFile "static/factionInfo.yaml" (Object res)

