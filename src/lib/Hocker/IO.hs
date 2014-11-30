module Hocker.IO (runCommands) where

import           Control.Monad   (void, (>=>))
import           Hocker.Commands
import           Hocker.Data
import           System.Exit     (ExitCode (..), exitWith)
import           System.IO       (hPutStrLn, stderr)
import           System.Process  (readProcessWithExitCode)


runCommands :: Flags -> [Command] -> IO ()
runCommands fs cmds = void $ cfold' cmds Nothing
  where
    cfold' :: [Command] -> Maybe String -> IO String
    cfold' []     Nothing  = return ""
    cfold' []     (Just x) = return x
    cfold' (c:cs) prev     = maybe
      (cfold' cs Nothing)
      (runCommand fs prev >=> (cfold' cs . Just))
      $ getCommand prev c

runCommand :: Flags -> Maybe String -> SysCommand -> IO String
runCommand fs prev cmd = do
  logM $  logCommand cmd fs
  out  <- maybeExecute fs cmd prev
  logM $  logOutput cmd out fs
  return $! out
  where
    logM = maybe (return ()) putStrLn

logCommand :: SysCommand -> Flags -> Maybe String
logCommand cmd (Flags {dryRun = True}) = Just ("simulating: " ++ show cmd)
logCommand cmd (Flags {quiet = q})
  | q < 2     = fmap (++ show cmd) $ case cmd of
    (Checking _)  -> Just "checking "
    _             -> Just "running "
  | otherwise = Nothing

logOutput :: SysCommand -> String -> Flags -> Maybe String
logOutput _ _   (Flags {dryRun = True}) = Nothing
logOutput (Checking _) _ _              = Nothing
logOutput _ out (Flags {quiet = q})
  | q < 1     = Just out
  | otherwise = Nothing

maybeExecute :: Flags -> SysCommand -> Maybe String -> IO String
maybeExecute (Flags {dryRun = True}) _ _ = return ""
maybeExecute _                  cmd prev = execute cmd prev

execute :: SysCommand -> Maybe String -> IO String
execute (Checking cmd) x = execute (Running cmd) x
execute (Running cmd)  _ =
  do
    (ec, out, err) <- execute' cmd
    exitWhenFailed ec err out
    return out
    where
      execute' c = readProcessWithExitCode (head c) (tail c) ""
      exitWhenFailed e@(ExitFailure _) err out = do
        putStrLn out
        hPutStrLn stderr err
        exitWith e
      exitWhenFailed _ _ _ = return ()
