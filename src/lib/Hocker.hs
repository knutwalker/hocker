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
  let parsedFlags = parseFlags args
  let configForHelp = either formatError formatConfig parsedConfig
  flags <- validateFlags configForHelp parsedFlags
  config <- validateConfig parsedConfig
  runHocker config flags

readConfig :: IO (Either ParseException Config)
readConfig = decodeFileEither "./Hockerfile"

formatError :: ParseException -> [String]
formatError (UnexpectedEvent (Just EventStreamEnd) _) = ["Config:", "Unexpected end of Hockerfile"]
formatError (InvalidYaml (Just (YamlException e))) = ["Config:", e]
formatError (AesonException e) = ["Config:", e]
formatError e = ["Config:", show e]

formatConfig :: Config -> [String]
formatConfig cfg =
  ["Config:"] ++
    showList' "  build      " (runBefore cfg) ++
    showStr   "  image      " (imageName cfg) ++
    showStr   "  dockerdir  " (dockerDirectory cfg) ++
    showMaybe "  container  " (containerName cfg) ++
    showMaybe "  hostname   " (hostName cfg) ++
    showList' "  ports      " (map show $ portMappings cfg) ++
    showList' "  arguments  " (startArgs cfg) ++
    showBool  "  daemonize  " (daemonized cfg)
  where
    showStr   name x        = [name ++ x]
    showMaybe _    Nothing  = []
    showMaybe name (Just x) = [name ++ x]
    showList' _    []       = []
    showList' name xs       = [name ++ unwords xs]
    showBool  name x        = [name ++ if x then "yes" else "no"]

validateConfig :: Either ParseException Config -> IO Config
validateConfig (Left e) = do
  hPutStrLn stderr $ (unlines . formatError) e
  exitWith $ ExitFailure 2
validateConfig (Right cfg) = return cfg

validateFlags :: [String] -> Either FError RunCommand -> IO RunCommand
validateFlags _   (Right r) = return r
validateFlags cfg (Left e) = do
  let (HelpOutput msg h ec) = usage cfg e
  hPutStrLn h msg
  exitWith ec

runHocker :: Config -> RunCommand -> IO ()
runHocker cfg (RunCommand flags action) = do
  let cmds = commands cfg action flags
  cfold flags cmds
