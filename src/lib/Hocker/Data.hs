{-# LANGUAGE OverloadedStrings #-}

module Hocker.Data where

import           Control.Applicative ((<$>), (<*>))
import           Control.Arrow       (first)
import qualified Data.HashMap.Strict as H
import           Data.List           (intercalate)
import           Data.Maybe          (fromMaybe, listToMaybe)
import           Data.Monoid         (Monoid, mappend, mempty)
import qualified Data.Text           as T
import           Data.Yaml
import           System.Exit         (ExitCode (..))
import           System.IO           (Handle, stdout)


data Flags = Flags
  { linux, dryRun  :: Bool
  , quiet, shallow :: Int
  } deriving (Show, Eq)

instance Monoid Flags where
  mempty = Flags
    { linux = False
    , dryRun = False
    , quiet  = 0
    , shallow = 0
    }
  mappend _ _ = undefined

withLinux :: Flags -> Flags
withLinux o = o { linux = True }

withDryRun :: Flags -> Flags
withDryRun o = o { dryRun = True }

withQuiet :: Flags -> Flags
withQuiet o = o { quiet = succ . quiet $ o }

withShallow :: Flags -> Flags
withShallow o = o { shallow = succ . shallow $ o }

data FError =
    Help
  | Version
  | NoAction
  | UnknownAction String
  | MultipleActions [String]
  | ParseError [String]
  deriving (Show, Eq)

data Action = Start | Stop | Restart | Status | Logs
  deriving (Show, Eq, Ord, Enum)

data RunCommand = RunCommand Flags Action
  deriving (Show, Eq)

data SysCommand =
    Running  String
  | Checking String

instance Show SysCommand where
  show (Running  cmd) = "'" ++ cmd ++ "'"
  show (Checking cmd) = "'" ++ cmd ++ "'"

data Command =
    Sure SysCommand
  | Unsure SysCommand (String -> Bool)

data Config = Config
  { imageName       :: String
  , containerName   :: String
  , dockerDirectory :: Maybe String
  , portMappings    :: [Int]
  , hostName        :: Maybe String
  , runBefore       :: Maybe String
  , daemonized      :: Bool
  , startArgs       :: Maybe String
  } deriving (Show, Eq)

instance FromJSON Config where
  parseJSON v =
    fromMaybe (fail "root image must be an object") $
      uncurry parseJson' <$>
      retrieveFirst v
    where
      retrieveFirst (Object o) = first T.unpack <$> listToMaybe (objectItems o)
      retrieveFirst _          = Nothing
      objectItems = filter (uncurry objectsOnly) . H.toList
      objectsOnly _ (Object _) = True
      objectsOnly _ _          = False
      parseJson' image (Object i) =
        Config image <$>
          i .:  "name"                 <*>
          i .:? "dockerfile"           <*>
          i .:? "ports"      .!= []    <*>
          i .:? "host"                 <*>
          i .:? "preStart"             <*>
          i .:? "background" .!= False <*>
          i .:? "startArgs"
      parseJson' img _ = fail $ "Image " ++ img ++ " must be an object"

minimalConfig :: String -> String -> Config
minimalConfig pImageName pContainerName = Config
  { runBefore       = Nothing
  , startArgs       = Nothing
  , imageName       = pImageName
  , containerName   = pContainerName
  , dockerDirectory = Nothing
  , portMappings    = []
  , hostName        = Nothing
  , daemonized      = False
  }

data HelpOutput = HelpOutput
  { message :: String
  , to      :: Handle
  , exit    :: ExitCode
  } deriving (Show)

instance Monoid HelpOutput where
  mempty = HelpOutput "" stdout ExitSuccess
  mappend a b =
    let m = intercalate "\n" [message a, message b]
    in  b { message = m }
