module Main where

import qualified Telegram.Lib as TG
import qualified VK.Lib as VK

main :: IO ()
main = do 
  --TG.startServer
  VK.startServer
