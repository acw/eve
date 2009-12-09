module Main where

import ItemDB
import System.Environment

main = do
  [shipA, shipB] <- getArgs
  c <- getContents
  case parseItemXml c of
    Left e -> do
       putStr "Parse failed: "
       putStrLn e
    Right a -> maybe (return ()) (mapM_ print) $ do
                 arbitrator <- findItem shipA a
                 vexor      <- findItem shipB a
                 return (itemDifferences (snd arbitrator) (snd vexor))
