import System.Environment (getArgs)

import Debian.Package.Build.Command (BuildMode (All))
import qualified Debian.Package.Build.Command as Command
import Debian.Package.Build
  (baseDirCurrent, defaultConfig, Build, runBuild, runIO,
   withCurrentDir, removeBuildDir, genSources)

build :: [String] -> Build ()
build opts = do
  removeBuildDir
  mayS <- genSources
  case mayS of
    Just (_, dir) -> withCurrentDir dir . runIO $ Command.build All opts
    Nothing       -> fail "Illegal state: genSources"

main :: IO ()
main =  do
  args <- getArgs
  runBuild (build args) baseDirCurrent defaultConfig
