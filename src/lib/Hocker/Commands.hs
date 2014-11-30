module Hocker.Commands (commands, getCommand) where

import           Control.Applicative ((<$>))
import           Control.Monad       (mfilter)
import           Data.List           (isInfixOf)
import           Hocker.Data


commands :: Config -> Action -> Flags -> [Command]
commands cfg action flags =
  let b = buildCommands cfg action flags
      v = vmCommands flags
      d = dockerCommands cfg action flags
  in b ++ v ++ d

getCommand :: Maybe String -> Command -> Maybe SysCommand
getCommand prev (Unsure cmd p) = const cmd <$> mfilter p prev
getCommand _    (Sure cmd)     = Just cmd

buildCommands :: Config -> Action -> Flags -> [Command]
buildCommands (Config { runBefore = cmd@(_:_) }) a fs =
  let shouldBuild = (`elem` [Start, Restart])
      wantToBuild f = not (shallow f || skip f)
  in  [build | shouldBuild a && wantToBuild fs]
  where
    build = running cmd
buildCommands _ _ _ = []

vmCommands :: Flags -> [Command]
vmCommands (Flags { linux = True }) = []
vmCommands _ = [check, ensure]
  where
    check = checking ["boot2docker", "status"]
    ensure = conditional ["boot2docker", "start"] ifRunning
    ifRunning = not . isInfixOf "running"

dockerCommands :: Config -> Action -> Flags -> [Command]
dockerCommands _ Start (Flags {shallow = True}) = docker "start"
dockerCommands cfg Start _ = map (docker' . ($cfg)) [buildDocker, runDocker]
dockerCommands _ Stop (Flags {shallow = s}) =
  let rm = if s then [] else docker "rm"
  in docker "stop" ++ rm
dockerCommands cfg Restart f = dockerCommands cfg Stop f ++ dockerCommands cfg Start f
dockerCommands _ Status _  = docker "ps"
dockerCommands _ Logs _   = docker "logs"

docker :: String -> [Command]
docker c = [docker' [c, "mc-neo4j"]]

docker' :: [String] -> Command
docker' = running . ("docker" :)

checking :: [String] -> Command
checking = Sure . Checking

running :: [String] -> Command
running = Sure . Running

conditional :: [String] -> (String -> Bool) -> Command
conditional = Unsure . Running

buildDocker :: Config -> [String]
buildDocker cfg = ["build", "-t=" ++ imageName cfg, dockerDirectory cfg]

runDocker :: Config -> [String]
runDocker cfg = ["run"] ++ optionalArgs cfg ++ [imageName cfg]

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
nameArgs (Config {containerName = Just n }) = ["--name", n]
nameArgs _ = []
