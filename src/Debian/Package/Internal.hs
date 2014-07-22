
module Debian.Package.Internal
       ( tarGz

       , splitCommand, handleExit

       , traceCommandIO, traceOutIO
       ) where

import Control.Arrow ((&&&))
import System.FilePath ((<.>))
import System.IO (Handle, hPutStrLn, hFlush, stderr)
import System.Exit (ExitCode (..))


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

handleExit :: String -> ExitCode -> IO ()
handleExit cmd = d  where
  d (ExitFailure rv) = fail $ unwords ["Failed with", show rv ++ ":", cmd]
  d  ExitSuccess     = return ()
