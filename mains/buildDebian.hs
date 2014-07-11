import System.Environment (getArgs)

import Debian.Package.Command (BuildMode (All))
import qualified Debian.Package.Command as Command
import Debian.Package.Source
import Debian.Package.Build
  (baseDirCurrent, defaultConfig,
   Build, runBuild, runIO, withCurrentDir, removeBuildDir, rsyncGenSources)

setupSrcDir :: Build FilePath
setupSrcDir =  do
  p            <- runIO $ parsePackageFromChangeLog Nothing
  srcDir       <- rsyncGenSources p
  return srcDir

build :: [String] -> Build ()
build opts = do
  removeBuildDir
  dir <- setupSrcDir
  withCurrentDir dir . runIO $ Command.build All opts

main :: IO ()
main =  do
  args <- getArgs
  runBuild (build args) baseDirCurrent defaultConfig
