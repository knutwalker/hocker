module Hocker (hocker) where

import           Data.Yaml
import           Hocker.Commands
import           Hocker.Data
import           Hocker.Flags
import           Hocker.IO
import           System.Exit     (ExitCode (..), exitWith)
import           System.IO       (hPutStrLn, stderr)
import           Text.Libyaml    (Event (..))


hocker :: [String] -> IO ()
hocker args = do
  parsedConfig <- readConfig
  let (validatedConfig, validatedFlags) = foldFlagsAndConfig parsedConfig args
  (RunCommand flags action) <- printInvalid validatedFlags
  config <- printInvalid validatedConfig
  let cmds = commands config action flags
  runCommands flags cmds

readConfig :: IO (Either ParseException Config)
readConfig = decodeFileEither "./Hockerfile"

foldFlagsAndConfig :: Either ParseException Config -> [String] -> (Either HelpOutput Config, Either HelpOutput RunCommand)
foldFlagsAndConfig parsedConfig args =
  let parsedFlags = parseFlags args
      configForHelp = either formatError formatConfig parsedConfig
      validatedFlags = validateFlags configForHelp parsedFlags
      validatedConfig = validateConfig parsedConfig
  in (validatedConfig, validatedFlags)

formatError :: ParseException -> [String]
formatError (UnexpectedEvent (Just EventStreamEnd) _) = ["Config:", "Unexpected end of Hockerfile"]
formatError (InvalidYaml (Just (YamlException e))) = ["Config:", e]
formatError (AesonException e) = ["Config:", e]
formatError e = ["Config:", show e]

formatConfig :: Config -> [String]
formatConfig cfg =
  ["Config:"] ++
    showStr   "  image      " (imageName cfg) ++
    showStr   "  container  " (containerName cfg) ++
    showStr   "  dockerdir  " (dockerDirectory cfg) ++
    showMaybe "  hostname   " (hostName cfg) ++
    showList' "  ports      " (map show $ portMappings cfg) ++
    showList' "  build      " (runBefore cfg) ++
    showList' "  arguments  " (startArgs cfg) ++
    showBool  "  daemonize  " (daemonized cfg)
  where
    showStr   name x        = [name ++ x]
    showMaybe _    Nothing  = []
    showMaybe name (Just x) = [name ++ x]
    showList' _    []       = []
    showList' name xs       = [name ++ unwords xs]
    showBool  name x        = [name ++ if x then "yes" else "no"]


validateFlags :: [String] -> Either FError RunCommand -> Either HelpOutput RunCommand
validateFlags = lmap . usage

validateConfig :: Either ParseException Config -> Either HelpOutput Config
validateConfig = lmap errorToHelp

errorToHelp :: ParseException -> HelpOutput
errorToHelp x = HelpOutput (unlines . formatError $ x) stderr (ExitFailure 2)

lmap :: (a -> b) -> Either a c -> Either b c
lmap _ (Right r) = Right r
lmap f (Left  l) = Left (f l)

printInvalid :: Either HelpOutput a -> IO a
printInvalid (Left (HelpOutput msg h ec)) = do
  hPutStrLn h msg
  exitWith ec
printInvalid (Right x) = return x
