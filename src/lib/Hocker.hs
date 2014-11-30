module Hocker (hocker) where

import           Data.Yaml
import           Hocker.Commands
import           Hocker.Data
import           Hocker.Flags
import           Hocker.IO
import           System.Exit     (exitWith)
import           System.IO       (hPutStrLn)


hocker :: [String] -> IO ()
hocker args = do
  let (parsedFlags, hockerLoc) = parseFlags args
  parsedConfig <- decodeFileEither hockerLoc
  let (validatedConfig, validatedFlags) = mergeConfigAndFlags parsedConfig parsedFlags
  (RunCommand fs action) <- printInvalid validatedFlags
  config <- printInvalid validatedConfig
  let cmds = commands config action fs
  runCommands fs cmds

printInvalid :: Either HelpOutput a -> IO a
printInvalid (Left (HelpOutput msg h ec)) = do
  hPutStrLn h msg
  exitWith ec
printInvalid (Right x) = return x
