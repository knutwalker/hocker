module Hocker.Flags (parseConfigAndFlags, usage) where

import           Data.Monoid           (mempty, (<>))
import           Data.Yaml
import           Hocker.Data
import           Hocker.Validation
import           System.Console.GetOpt
import           System.Exit           (ExitCode (..))
import           System.IO             (stderr, stdout)


parseFlags :: [String] -> Either FError RunCommand
parseFlags args =
  let (os, as, es) = getOpt Permute options args
      opts         = foldr ($) (Right mempty) os
      action       = validateActions as
      errors       = validateErrors es
  in  do
    o <- opts
    _ <- errors
    a <- action
    return $! RunCommand o a

options :: [OptDescr (Either FError Flags -> Either FError Flags)]
options =
  [ Option "sw" ["shallow", "skip"]
    (NoArg $ fmap withShallow)
    "Skip pre-start steps--One skips the pre build, two skip the docker build"
  , Option "nd"  ["dry-run", "noop"]
    (NoArg $ fmap withDryRun)
    "Only show what would have been done, but don't do it"
  , Option "q"  ["quiet"]
    (NoArg $ fmap withQuiet)
    "Less chatter (twice for even lesser chatter)"
  , Option "l"  ["linux"]
    (NoArg $ fmap withLinux)
    "When on linux, skip checking for boot2docker"
  , Option "vV" ["version"]
    (NoArg $ const (Left Version))
    "Show the version and exit"
  , Option "h?" ["help"]
    (NoArg $ const (Left Help))
    "Show this help and exit"
  ]

{------- help formatting ----------}

usage :: [String] -> FError -> HelpOutput
usage cfg  Help                = (usage cfg (ParseError [])) { to = stdout, exit = ExitSuccess }
usage _    Version             = succeedWith (progName ++ " " ++ version)
  where progName = "hocker"
        version  = "v0.8.0"
usage cfg  NoAction            = usage cfg Version <> (usage cfg Help) { to = stderr }
usage _   (UnknownAction a)    = failWith $ "Unkown action: " ++ a
usage _   (MultipleActions as) = failWith msg
  where msg = unlines ["Multiple actions given (" ++ unwords as ++ ")", "Can only do one at a time"]
usage cfg (ParseError es)      = failWith $ formatUsage es options cfg


validateFlags :: [String] -> Either FError RunCommand -> Either HelpOutput RunCommand
validateFlags = lmap . usage

validateConfig :: Either ParseException Config -> Either HelpOutput Config
validateConfig = lmap errorToHelp

errorToHelp :: ParseException -> HelpOutput
errorToHelp x = HelpOutput (unlines . formatYamlError $ x) stderr (ExitFailure 2)

parseConfigAndFlags :: Either ParseException Config -> [String] -> (Either HelpOutput Config, Either HelpOutput RunCommand)
parseConfigAndFlags parsedConfig args =
  let parsedFlags = parseFlags args
      configForHelp = formatParsedConfig parsedConfig
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
