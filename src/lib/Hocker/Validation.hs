{-# LANGUAGE FlexibleInstances #-}

module Hocker.Validation where

import           Data.Char             (toLower)
import           Data.List             (intercalate)
import           Data.Yaml
import           Hocker.Data
import           System.Console.GetOpt (OptDescr, usageInfo)
import           Text.Libyaml          (Event (..))


class Format a where
  format :: a -> Maybe String
  formats :: String -> a -> [String]
  formats name c = maybe [] ((:[]) . (name ++)) $ format c

instance Format String where
  format = Just

instance Format (Maybe String) where
  format = id

instance Format Bool where
  format b = Just $ if b then "yes" else "no"

instance Format [String] where
  format [] = Nothing
  format xs = Just $ unwords xs


validateActions :: [String] -> Either FError Action
validateActions [a] = parseAction a
  where
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
validateActions []  = Left   NoAction
validateActions as  = Left  (MultipleActions as)

validateErrors :: [String] -> Either FError ()
validateErrors []  = Right ()
validateErrors es  = Left  (ParseError es)

formatParsedConfig :: Either ParseException Config -> [String]
formatParsedConfig = either formatYamlError formatConfig

formatYamlError :: ParseException -> [String]
formatYamlError (UnexpectedEvent (Just EventStreamEnd) _) = ["Config:", "Unexpected end of Hockerfile"]
formatYamlError (InvalidYaml (Just (YamlException e))) = ["Config:", e]
formatYamlError (AesonException e) = ["Config:", e]
formatYamlError e = ["Config:", show e]

formatConfig :: Config -> [String]
formatConfig cfg =
  ["Config:"] ++
    formats "  image      " (imageName cfg) ++
    formats "  container  " (containerName cfg) ++
    formats "  dockerdir  " (dockerDirectory cfg) ++
    formats "  hostname   " (hostName cfg) ++
    formats "  ports      " (map show $ portMappings cfg) ++
    formats "  build      " (runBefore cfg) ++
    formats "  arguments  " (startArgs cfg) ++
    formats "  background " (daemonized cfg)

formatActions :: [String]
formatActions =
  [ "", "Actions:"
  , "  start    Start the docker container"
  , "  stop     Stop the docker container"
  , "  restart  Restart the docker container"
  , "  status   Print the container's status"
  , "  logs     Print the container's log file" ]

formatHeader :: String
formatHeader = intercalate "\n"
  [ "Usage: hocker [options] action", ""
  , "Options:" ]

formatUsage :: [String] -> [OptDescr a] -> [String] -> String
formatUsage es options cfg = concat es ++ usageInfo formatHeader options ++ unlines (formatActions ++ [""] ++ cfg)
