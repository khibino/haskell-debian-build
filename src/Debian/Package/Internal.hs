
module Debian.Package.Internal (
  tarGz,

  readProcess', rawSystem', system',

  traceCommandIO, traceOutIO
  ) where

import Control.Arrow ((&&&))
import System.FilePath ((<.>))
import System.IO (Handle, hPutStrLn, hFlush, stderr)
import System.Exit (ExitCode (..))
import System.Process (readProcess, rawSystem, system)


tarGz :: String
tarGz =  "tar" <.> "gz"

trace' :: Handle -> Char -> String -> IO ()
trace' fh pc s = do
  hPutStrLn fh $ pc : " " ++ s
  hFlush fh

trace :: Char -> String -> IO ()
trace =  trace' stderr

traceCommandIO :: String -> IO ()
traceCommandIO =  trace '+'

traceOutIO :: String -> IO ()
traceOutIO =  trace '>'

splitCommand :: [a] -> (a, [a])
splitCommand =  head &&& tail

readProcess' :: [String] -> IO String
readProcess' cmd0 = do
  let (cmd, args) = splitCommand cmd0
  readProcess cmd args ""

handleExit :: String -> ExitCode -> IO ()
handleExit cmd = d  where
  d (ExitFailure rv) = fail $ unwords ["Failed with", show rv ++ ":", cmd]
  d  ExitSuccess     = return ()

rawSystem' :: [String] -> IO ()
rawSystem' cmd0 = do
  traceCommandIO $ unwords cmd0
  let (cmd, args) = splitCommand cmd0
  rawSystem cmd args >>= handleExit cmd

system' :: String -> IO ()
system' cmd = do
  traceCommandIO cmd
  system cmd >>= handleExit cmd
