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
  parsedConfig <- readConfig
  let (validatedConfig, validatedFlags) = parseConfigAndFlags parsedConfig args
  (RunCommand flags action) <- printInvalid validatedFlags
  config <- printInvalid validatedConfig
  let cmds = commands config action flags
  runCommands flags cmds

readConfig :: IO (Either ParseException Config)
readConfig = decodeFileEither "./Hockerfile"

printInvalid :: Either HelpOutput a -> IO a
printInvalid (Left (HelpOutput msg h ec)) = do
  hPutStrLn h msg
  exitWith ec
printInvalid (Right x) = return x
