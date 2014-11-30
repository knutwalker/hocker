module Hocker.Commands (commands, getCommand) where

import           Control.Applicative ((<$>))
import           Control.Monad       (mfilter)
import           Data.List           (isInfixOf)
import           Data.Maybe          (fromMaybe, mapMaybe)
import           Hocker.Data
import           Text.Printf


commands :: Config -> Action -> Flags -> [Command]
commands cfg action flags =
  let b = builds cfg action flags
      v = vms flags
      d = dockers cfg action flags
  in b ++ v ++ d

getCommand :: Maybe String -> Command -> Maybe SysCommand
getCommand prev (Unsure cmd p) = const cmd <$> mfilter p prev
getCommand _    (Sure cmd)     = Just cmd

builds :: Config -> Action -> Flags -> [Command]
builds (Config { runBefore = Just cmd }) a fs =
  let shouldBuild = (`elem` [Start, Restart])
      wantToBuild = (<1) . shallow
  in  [running cmd | shouldBuild a && wantToBuild fs]
builds _ _ _ = []

vms :: Flags -> [Command]
vms (Flags { linux = True }) = []
vms _ = [check, ensure]
  where
    check = checking "boot2docker status"
    ensure = conditional "boot2docker start" ifRunning
    ifRunning = not . isInfixOf "running"

dockers :: Config -> Action -> Flags -> [Command]
dockers cfg Stop   fs | shallow fs >= 2     = docker "stop"  cfg
dockers cfg Start  fs | shallow fs >= 2     = docker "start" cfg
dockers cfg Status _                        = docker "ps"    cfg
dockers cfg Logs   _                        = docker "logs"  cfg
dockers cfg Stop   _  = docker "stop" cfg  ++ docker "rm" cfg
dockers cfg Start  _  = buildDocker cfg    ++ runDocker cfg
dockers cfg Restart f = dockers cfg Stop f ++ dockers cfg Start f

docker :: String -> Config -> [Command]
docker c cfg = [docker' $ printf "%s %s" c (containerName cfg)]

docker' :: String -> Command
docker' = running . ("docker " ++ )

checking :: String -> Command
checking = Sure . Checking

running :: String -> Command
running = Sure . Running

conditional :: String -> (String -> Bool) -> Command
conditional = Unsure . Running

buildDocker :: Config -> [Command]
buildDocker cfg = fromMaybe [] $ (:[]) . docker' . printf  "build -t=\"%s\" \"%s\"" (imageName cfg) <$> dockerDirectory cfg

runDocker :: Config -> [Command]
runDocker cfg = [docker' $ printf "run %s \"%s\"" (optionalArgs cfg) (imageName cfg)]

optionalArgs :: Config -> String
optionalArgs cfg = unwords . mapMaybe ($cfg) $ [daemonArgs, portArgs, hostArgs, startArgs, nameArgs]

daemonArgs :: Config -> Maybe String
daemonArgs (Config {daemonized = True }) = Just "-d"
daemonArgs _ = Nothing

portArgs :: Config -> Maybe String
portArgs (Config {portMappings = [] }) = Nothing
portArgs (Config {portMappings = ps }) =
  Just . unwords $ map portArg ps
  where portArg p = printf "-p %d:%d" p p

hostArgs :: Config -> Maybe String
hostArgs = fmap (printf "-h \"%s\"") . hostName

nameArgs :: Config -> Maybe String
nameArgs = Just . printf "--name \"%s\"" . containerName
