
module Debian.Package.Internal
       ( tarGz

       , traceCommandIO, traceOutIO
       ) where

import System.FilePath ((<.>))
import System.IO (Handle, hPutStrLn, hFlush, stderr)


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
