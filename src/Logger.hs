
module Logger (Log(..), logFunctions) where

import Config (Priority (..))

{-
logs funcctions
-}
data Log = Log
  { logDebug :: String -> IO (),
    logInfo :: String -> IO (),
    logWarn :: String -> IO ()
  }

{-
-- Debug < Info : True 
Debug (print, print, print)
Info (logEmpty, print, print)
Warn (logEmpty, logEmpty, print)
-}
logPriority :: Priority -> Priority -> String -> IO ()
logPriority i p s | i <= p = putStrLn $ show p ++ " | " ++ s
                  | otherwise = return ()

-- return logs funcctions
logFunctions :: Priority -> Log
logFunctions p =
  Log
    { logDebug = logPriority p Debug,
      logInfo = logPriority p Info,
      logWarn = logPriority p Warn
    }
