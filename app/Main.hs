module Main where

import Config (Config (..))
import qualified Data.ByteString as B
import qualified Data.Yaml as Y
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (getGlobalManager, setGlobalManager, tlsManagerSettings)
import qualified Telegram.Lib as TG
import qualified VK.Lib as VK

{-
старт пока только одного бота за раз,
чтобы не парится с их безопасной остановкой
-}

main :: IO ()
main = do
  -- парсим конфиг
  Config {..} <- Y.decodeThrow =<< B.readFile "src/Config.yaml"
  -- запуск тлс менеджера
  manager <- newManager tlsManagerSettings
  setGlobalManager manager
  -- запуск бота
  if tgStart
    then TG.startServer tgConfig settings
    else
      if vkStart
        then VK.startServer vkConfig settings
        else print "NOBODY START TODAY"
