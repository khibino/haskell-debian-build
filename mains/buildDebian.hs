import System.Environment (getArgs)

import Debian.Package.Build
  (BuildMode (All), buildPackage,
   baseDirCurrent, defaultConfig, Build, runBuild, liftTrace,
   removeBuildDir, genSources)

build :: [String] -> Build ()
build opts = do
  removeBuildDir
  mayS <- genSources
  case mayS of
    Just (_, dir) -> liftTrace $ buildPackage dir All opts
    Nothing       -> fail "Illegal state: genSources"

main :: IO ()
main =  do
  args <- getArgs
  runBuild (build args) baseDirCurrent defaultConfig
