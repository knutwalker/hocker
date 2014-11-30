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
  { skip, shallow, linux, dryRun :: Bool
  , quiet                        :: Int
  } deriving (Show, Eq)

instance Monoid Flags where
  mempty = Flags
    { skip  = False
    , shallow = False
    , linux = False
    , dryRun = False
    , quiet  = 0
    }
  mappend _ _ = undefined

withSkip :: Flags -> Flags
withSkip o = o { skip = True }

withShallow :: Flags -> Flags
withShallow o = o { shallow = True }

withLinux :: Flags -> Flags
withLinux o = o { linux = True }

withDryRun :: Flags -> Flags
withDryRun o = o { dryRun = True }

withQuiet :: Flags -> Flags
withQuiet o = o { quiet = succ . quiet $ o }

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
    Running  [String]
  | Checking [String]

instance Show SysCommand where
  show (Running  cmd) = "'" ++ unwords cmd ++ "'"
  show (Checking cmd) = "'" ++ unwords cmd ++ "'"

data Command =
    Sure SysCommand
  | Unsure SysCommand (String -> Bool)

data Config = Config
  { imageName       :: String
  , containerName   :: String
  , dockerDirectory :: String
  , portMappings    :: [Int]
  , hostName        :: Maybe String
  , runBefore       :: [String]
  , daemonized      :: Bool
  , startArgs       :: [String]
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
          i .:  "container"            <*>
          i .:? "dockerfile" .!= "."   <*>
          i .:? "ports"      .!= []    <*>
          i .:? "host"                 <*>
          i .:: "preStart"             <*>
          i .:? "daemonized" .!= False <*>
          i .:: "startArgs"
      parseJson' img _ = fail $ "Image " ++ img ++ " must be an object"
      x .:: n = words <$> x .:? n .!= ""

minimalConfig :: String -> String -> Config
minimalConfig pImageName pContainerName = Config
  { runBefore       = []
  , startArgs       = []
  , imageName       = pImageName
  , containerName   = pContainerName
  , dockerDirectory = "."
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
