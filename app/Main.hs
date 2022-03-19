module Main where

import Config (Config (..))
import qualified Data.ByteString as B
import qualified Data.Yaml as Y
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (getGlobalManager, setGlobalManager, tlsManagerSettings)
import qualified Telegram.Lib as TG
import qualified VK.Lib as VK

{-
start only one bot at a time,
not to bother with their safe stop
-}

main :: IO ()
main = do
  -- parse config
  Config {..} <- Y.decodeThrow =<< B.readFile "src/Config.yaml"
  -- start tls manager
  manager <- newManager tlsManagerSettings
  setGlobalManager manager
  -- start bot
  if tgStart
    then TG.startServer tgConfig settings
    else
      if vkStart
        then VK.startServer vkConfig settings
        else print "NOBODY START TODAY"
