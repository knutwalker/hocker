module Hocker.Flags (parseFlags, usage) where

import           Data.Char             (toLower)
import           Data.List             (intercalate)
import           Data.Monoid           (mempty, (<>))
import           Hocker.Data
import           System.Console.GetOpt
import           System.Exit           (ExitCode (..))
import           System.IO             (stderr, stdout)


parseFlags :: [String] -> Either FError RunCommand
parseFlags args =
  let (os, as, es) = getOpt Permute options args
      opts         = foldr ($) (Right mempty) os
      action       = validateAction as
      errors       = validateErrors es
  in  do
    o <- opts
    _ <- errors
    a <- action
    return $! RunCommand o a
  where
    validateAction :: [String] -> Either FError Action
    validateAction [a] = parseAction a
    validateAction []  = Left   NoAction
    validateAction as  = Left  (MultipleActions as)
    parseAction :: String -> Either FError Action
    parseAction s = maybe (Left (UnknownAction s)) Right $ readAction s
    readAction :: String -> Maybe Action
    readAction s = case map toLower s of
      "start"   -> Just Start
      "stop"    -> Just Stop
      "restart" -> Just Restart
      "status"  -> Just Status
      "logs"    -> Just Logs
      _         -> Nothing
    validateErrors :: [String] -> Either FError ()
    validateErrors []  = Right ()
    validateErrors es  = Left  (ParseError es)

options :: [OptDescr (Either FError Flags -> Either FError Flags)]
options =
  [ Option "s"  ["skip", "no-build"]
    (NoArg $ fmap withSkip)
    "Skip the rebuild of the CypherRs"
  , Option "w"  ["shallow"]
    (NoArg $ fmap withShallow)
    "Do not rebuild the container (just start/stop)"
  , Option "nd"  ["dry-run", "noop"]
    (NoArg $ fmap withDryRun)
    "Only show what would have been done, but don't do it"
  , Option "q"  ["quiet"]
    (NoArg $ fmap withQuiet)
    "Less chatter (twice for even lesser chatter)"
  , Option "l"  ["linux"]
    (NoArg $ fmap withLinux)
    "When on linux, skip checking boot2docker"
  , Option "vV" ["version"]
    (NoArg $ const (Left Version))
    "Show the version and exit"
  , Option "h?" ["help"]
    (NoArg $ const (Left Help))
    "Show this help and exit"
  ]

usage :: [String] -> FError -> HelpOutput
usage cfg  Help                = (usage cfg (ParseError [])) { to = stdout, exit = ExitSuccess }
usage _    Version             = succeedWith (progName ++ " " ++ version)
  where progName = "hocker"
        version  = "v0.5.0"
usage cfg  NoAction            = usage cfg Version <> (usage cfg Help) { to = stderr }
usage _   (UnknownAction a)    = failWith $ "Unkown action: " ++ a
usage _   (MultipleActions as) = failWith msg
  where msg = unlines ["Multiple actions given (" ++ unwords as ++ ")", "Can only do one at a time"]
usage cfg (ParseError es)      = failWith msg
  where
    msg =
      concat es ++ usageInfo header options ++ unlines (showActions ++ [""] ++ cfg)
    header = intercalate "\n"
      [ "Usage: hocker [options] action", ""
      , "Options:" ]
    showActions =
      [ "", "Actions:"
      , "  start    Start the docker container"
      , "  stop     Stop the docker container"
      , "  restart  Restart the docker container"
      , "  status   Print the container's status"
      , "  logs     Print the container's log file" ]

failWith :: String -> HelpOutput
failWith s = HelpOutput s stderr (ExitFailure 1)


succeedWith :: String -> HelpOutput
succeedWith s = mempty { message = s }
