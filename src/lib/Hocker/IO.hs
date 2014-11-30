module Hocker.IO (runCommands) where

import           Control.Monad   (void, when, (>=>))
import           GHC.IO.Handle   (hDuplicate, hGetContents)
import           Hocker.Commands
import           Hocker.Data
import           System.Exit     (ExitCode (..), exitWith)
import           System.IO       (hPutStrLn, stderr)
import           System.Process  hiding (runCommand)


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
  logM $ logCommand cmd fs
  out <- maybeExecute (shouldLogOutput cmd fs) fs cmd prev
  return $! out
  where
    logM = maybe (return ()) putStrLn

logCommand :: SysCommand -> Flags -> Maybe String
logCommand cmd (Flags {dryRun = True}) =
                  Just $ "simulating: " ++ show cmd
logCommand cmd (Flags {quiet = q})
  | q < 1     = case cmd of
    (Checking _) -> Just $ "checking: " ++ show cmd
    _            -> Just $  "running: " ++ show cmd
  | otherwise = Nothing

shouldLogOutput :: SysCommand -> Flags -> Bool
shouldLogOutput _ (Flags {dryRun = True}) = False
shouldLogOutput (Checking _) _            = False
shouldLogOutput _ (Flags {quiet = q})     = q < 2

maybeExecute :: Bool -> Flags -> SysCommand -> Maybe String -> IO String
maybeExecute _    (Flags {dryRun = True}) _ _ = return ""
maybeExecute logs _                  cmd prev = execute logs cmd prev

execute :: Bool -> SysCommand -> Maybe String -> IO String
execute logs (Checking cmd) x = execute logs (Running cmd) x
execute logs (Running cmd)  _ = do
  (ec, out) <- call logs cmd
  exitWhenFailed ec
  return out
  where
    exitWhenFailed e@(ExitFailure _) = exitWith e
    exitWhenFailed _                 = return ()

call :: Bool -> String -> IO (ExitCode, String)
call logs c =
  let cpOpts = (shell c) { std_out = CreatePipe
                         , std_err = CreatePipe
                         , delegate_ctlc = True }
  in do
    (_, Just hout, Just herr, ph) <- createProcess cpOpts
    hdout                         <- hDuplicate hout

    when logs $ do
      outbuf <- hGetContents hout
      mapM_ putStrLn (lines outbuf)
      errbuf <- hGetContents herr
      mapM_ (hPutStrLn stderr) (lines errbuf)

    out <- hGetContents hdout
    ex  <- waitForProcess ph

    return (ex, out)
