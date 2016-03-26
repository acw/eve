module Fix where

import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import Data.Text(unpack,pack)
import Data.Yaml hiding(Value)
import Database.SQLite

processRows :: [[Row Value]] -> M.Map String Word
processRows rs = foldr convert M.empty (concat rs)
 where
  convert [("solarSystemID", Int x), ("solarSystemName", Text str)] m = M.insert str (fromIntegral x) m
  convert _ _ = error "Bad row"

run :: IO ()
run =
  do sql <- openReadonlyConnection "static/universeDataDx.db"
     Just (Object o) <- decodeFile "static/planetInfo.yaml"
     Right rows <- execStatement sql "SELECT solarSystemID, solarSystemName FROM mapSolarSystems"
     let rows' = processRows rows
         o' = HM.toList o
         o'' = map (\ (n,v) ->
                      case M.lookup (unpack n) rows' of
                        Just n' -> (pack (show n'), v)
                        Nothing -> error ("Couldn't find name: " ++ unpack n)) o'
     encodeFile "static/systemInfo.yaml" (Object (HM.fromList o''))
