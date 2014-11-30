module Hocker.Commands (commands, getCommand) where

import           Control.Applicative ((<$>))
import           Control.Monad       (mfilter)
import           Data.List           (isInfixOf)
import           Hocker.Data


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
builds (Config { runBefore = cmd@(_:_) }) a fs =
  let shouldBuild = (`elem` [Start, Restart])
      wantToBuild f = not (shallow f || skip f)
  in  [running cmd | shouldBuild a && wantToBuild fs]
builds _ _ _ = []

vms :: Flags -> [Command]
vms (Flags { linux = True }) = []
vms _ = [check, ensure]
  where
    check = checking ["boot2docker", "status"]
    ensure = conditional ["boot2docker", "start"] ifRunning
    ifRunning = not . isInfixOf "running"

dockers :: Config -> Action -> Flags -> [Command]
dockers cfg Stop  (Flags {shallow = True})  = docker "stop"  cfg
dockers cfg Start (Flags {shallow = True})  = docker "start" cfg
dockers cfg Status _                        = docker "ps"    cfg
dockers cfg Logs   _                        = docker "logs"  cfg
dockers cfg Stop   _  = docker "stop" cfg  ++ docker "rm" cfg
dockers cfg Start  _  = [buildDocker cfg    , runDocker cfg]
dockers cfg Restart f = dockers cfg Stop f ++ dockers cfg Start f

docker :: String -> Config -> [Command]
docker c cfg = [docker' [c, containerName cfg]]

docker' :: [String] -> Command
docker' = running . ("docker" :)

checking :: [String] -> Command
checking = Sure . Checking

running :: [String] -> Command
running = Sure . Running

conditional :: [String] -> (String -> Bool) -> Command
conditional = Unsure . Running

buildDocker :: Config -> Command
buildDocker cfg = docker' ["build", "-t=" ++ imageName cfg, dockerDirectory cfg]

runDocker :: Config -> Command
runDocker cfg = docker' (["run"] ++ optionalArgs cfg ++ [imageName cfg])

optionalArgs :: Config -> [String]
optionalArgs cfg = concatMap ($cfg) [daemonArgs, portArgs, hostArgs, startArgs, nameArgs]

daemonArgs :: Config -> [String]
daemonArgs (Config {daemonized = True }) = ["-d"]
daemonArgs _ = []

portArgs :: Config -> [String]
portArgs (Config {portMappings = ps }) =
  concatMap portArg ps
  where portArg p = ["-p", show p ++ ":" ++ show p]

hostArgs :: Config -> [String]
hostArgs (Config {hostName = Just h }) = ["-h", h]
hostArgs _ = []

nameArgs :: Config -> [String]
nameArgs (Config {containerName = n }) = ["--name", n]
