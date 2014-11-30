module Hocker.Flags (mergeConfigAndFlags, parseFlags, usage) where

import           Data.Char             (toLower)
import           Data.Maybe            (fromMaybe)
import           Data.Monoid           (mempty, (<>))
import           Data.Yaml
import           Hocker.Data
import           Hocker.Validation
import           Hocker.Version
import           System.Console.GetOpt
import           System.Exit           (ExitCode (..))
import           System.IO             (stderr, stdout)


parseFlags :: [String] -> (Either FError RunCommand, String)
parseFlags args =
  let fs = parseFlags' args
      hl = hockerFileLoc fs
  in  (fs, fromMaybe "./Hockerfile" hl)

parseFlags' :: [String] -> Either FError RunCommand
parseFlags' args =
  let (os, as, es) = getOpt Permute options args
      opts         = foldr ($) (Right mempty) os
      fallback     = either flags id opts
      action       = validateActions as fallback
      errors       = validateErrors es fallback
  in  do
    o <- opts
    _ <- errors
    a <- action
    return $! RunCommand o a
  where
    validateActions :: [String] -> Flags -> Either FError Action
    validateActions [a] o = parseAction a o
    validateActions []  o = Left (NoAction o)
    validateActions as  o = Left (MultipleActions as o)

    parseAction :: String -> Flags -> Either FError Action
    parseAction s o = maybe (Left (UnknownAction s o)) Right $ readAction s

    readAction :: String -> Maybe Action
    readAction s = case map toLower s of
      "start"   -> Just Start
      "stop"    -> Just Stop
      "restart" -> Just Restart
      "status"  -> Just Status
      "logs"    -> Just Logs
      _         -> Nothing

    validateErrors :: [String] -> Flags -> Either FError ()
    validateErrors [] _ = Right ()
    validateErrors es o = Left  (ParseError es o)

options :: [OptDescr AddFlag]
options =
  [ Option "sw" ["shallow", "skip"]    (NoArg setShallow)
    "Skip pre-start steps--One skips the pre build, two skip the docker build"
  , Option "nd"  ["dry-run", "noop"]   (NoArg setDryRun)
    "Only show what would have been done, but don't do it"
  , Option "q"  ["quiet"]              (NoArg setQuiet)
    "Less chatter (twice for even lesser chatter)"
  , Option "l"  ["linux"]              (NoArg setLinux)
    "When on linux, skip checking for boot2docker"
  , Option "f"  ["file", "hockerfile"] (ReqArg setHockerFile "FILE")
    "Specify the Hockerfile to use (default is Hockerfile)"
  , Option "vV" ["version"]            (NoArg setVersion)
    "Show the version and exit"
  , Option "h?" ["help"]               (NoArg setHelp)
    "Show this help and exit"
  ]

{------- help formatting ----------}

usage :: [String] -> FError -> HelpOutput
usage cfg (Help fs)              = (usage cfg (ParseError [] fs)) { to = stdout, exit = ExitSuccess }
usage _   (Version _)            = succeedWith (programName ++ " " ++ programVersion)
usage cfg (NoAction fs)          = usage cfg (Version fs) <> (usage cfg (Help fs)) { to = stderr }
usage _   (UnknownAction a _)    = failWith $ "Unkown action: " ++ a
usage _   (MultipleActions as _) = failWith msg
  where msg = unlines ["Multiple actions given (" ++ unwords as ++ ")", "Can only do one at a time"]
usage cfg (ParseError es _)      = failWith $ formatUsage es options cfg

validateFlags :: [String] -> Either FError RunCommand -> Either HelpOutput RunCommand
validateFlags = lmap . usage

validateConfig :: Either ParseException Config -> Either HelpOutput Config
validateConfig = lmap errorToHelp

errorToHelp :: ParseException -> HelpOutput
errorToHelp x = HelpOutput (unlines . formatYamlError $ x) stderr (ExitFailure 2)

hockerFileLoc :: Either FError RunCommand -> Maybe String
hockerFileLoc (Right (RunCommand fs _)) = hockerFile fs
hockerFileLoc (Left x) = hockerFile . flags $ x

mergeConfigAndFlags :: Either ParseException Config -> Either FError RunCommand -> (Either HelpOutput Config, Either HelpOutput RunCommand)
mergeConfigAndFlags parsedConfig parsedFlags =
  let configForHelp = formatParsedConfig parsedConfig
      validatedFlags = validateFlags configForHelp parsedFlags
      validatedConfig = validateConfig parsedConfig
  in (validatedConfig, validatedFlags)

failWith :: String -> HelpOutput
failWith s = HelpOutput s stderr (ExitFailure 1)

succeedWith :: String -> HelpOutput
succeedWith s = mempty { message = s }

lmap :: (a -> b) -> Either a c -> Either b c
lmap _ (Right r) = Right r
lmap f (Left  l) = Left (f l)
