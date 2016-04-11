{-# LANGUAGE TemplateHaskell #-}
module EVE.Console(
         Console
       , conFlush, conPut
       , stdoutConsole, stderrConsole, nullConsole
       , logs
       )
 where

import Control.Lens.TH(makeLenses)
import System.IO(hPutStr, hFlush, stderr, stdout)

data Console = Console {
       _conPut   :: String -> IO ()
     , _conFlush :: IO ()
     }

makeLenses ''Console

logs :: Console -> String -> IO ()
logs c s = _conPut c s >> _conFlush c

stdoutConsole :: Console
stdoutConsole = Console (hPutStr stdout) (hFlush stdout)

stderrConsole :: Console
stderrConsole = Console (hPutStr stderr) (hFlush stderr)

nullConsole :: Console
nullConsole = Console (const (return ())) (return ())


