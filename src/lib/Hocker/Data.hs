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
  , hockerFile     :: Maybe String
  } deriving (Show, Eq)

instance Monoid Flags where
  mempty = Flags
    { linux = False
    , dryRun = False
    , quiet  = 0
    , shallow = 0
    , hockerFile = Nothing
    }
  mappend _ _ = undefined

type AddFlag = Either FError Flags -> Either FError Flags

setLinux :: AddFlag
setLinux = lrmap (\o -> o { linux = True })

setDryRun :: AddFlag
setDryRun = lrmap (\o -> o { dryRun = True })

setQuiet :: AddFlag
setQuiet = lrmap (\o -> o { quiet = succ . quiet $ o })

setShallow :: AddFlag
setShallow = lrmap (\o -> o { shallow = succ . shallow $ o })

setHockerFile :: String -> AddFlag
setHockerFile h = lrmap (\o -> o { hockerFile = Just h })

setHelp :: AddFlag
setHelp (Right fs) = Left (Help fs)
setHelp (Left fs) = Left (Help (flags fs))

setVersion :: AddFlag
setVersion (Right fs) = Left (Version fs)
setVersion (Left fs) = Left (Version (flags fs))

lrmap :: (Flags -> Flags) -> AddFlag
lrmap f (Left e) = Left $ flagMap f e
lrmap f (Right x) = Right $ flagMap f x


data FError =
    Help Flags
  | Version Flags
  | NoAction Flags
  | UnknownAction String Flags
  | MultipleActions [String] Flags
  | ParseError [String] Flags
  deriving (Show, Eq)

class FlagContainer a where
  flags :: a -> Flags
  flagMap :: (Flags -> Flags) -> a -> a

instance FlagContainer Flags where
  flags = id
  flagMap f = f

instance FlagContainer FError where
  flags (Help fs)              = fs
  flags (Version fs)           = fs
  flags (NoAction fs)          = fs
  flags (UnknownAction _ fs)   = fs
  flags (MultipleActions _ fs) = fs
  flags (ParseError _ fs)      = fs

  flagMap f (Help fs)               = Help               $ f fs
  flagMap f (Version fs)            = Version            $ f fs
  flagMap f (NoAction fs)           = NoAction           $ f fs
  flagMap f (UnknownAction a fs)    = UnknownAction a    $ f fs
  flagMap f (MultipleActions as fs) = MultipleActions as $ f fs
  flagMap f (ParseError es fs)      = ParseError es      $ f fs

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
