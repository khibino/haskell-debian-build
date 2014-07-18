
module Debian.Package.Build.Command (
  chdir, pwd,
  renameDirectory, renameFile,

  confirmPath,

  unpackInDir, unpack, packInDir', packInDir,

  cabalDebian,

  debuild,

  BuildMode (..),

  buildPackage, rebuild,

  reinstallGhcLibrary,

  withCurrentDir,

  getBaseDir, withBaseCurrentDir,
  getBuildDir, withBuildDir
  ) where

import Data.Maybe (fromMaybe)
import Control.Applicative ((<$>))
import Control.Monad (when)
import System.FilePath ((<.>), takeDirectory)
import qualified System.Directory as D

import Debian.Package.Internal
  (tarGz, readProcess', rawSystem', system', traceCommandIO, traceOutIO)
import Debian.Package.Hackage (Hackage, ghcLibraryBinPackages, ghcLibraryPackages)
import Debian.Package.Build.Monad
  (Build, runIO, askBaseDir, askBuildDir, askConfig, trace)


traceBuild :: IO () -> Build ()
traceBuild printIO = do
 t <- trace <$> askConfig
 when t $ runIO printIO

traceCommand :: String -> Build ()
traceCommand =  traceBuild . traceCommandIO

traceOut :: String -> Build ()
traceOut =  traceBuild . traceOutIO

readProcess :: [String] -> Build String
readProcess cmd = do
  traceCommand $ unwords cmd
  runIO $ readProcess' cmd

rawSystem :: [String] -> Build ()
rawSystem cmd = do
  traceCommand $ unwords cmd
  runIO . rawSystem' $ cmd

system :: String -> Build ()
system cmd = do
  traceCommand cmd
  runIO $ system' cmd

chdir :: String -> Build ()
chdir dir =  do
  traceCommand $ "<setCurrentDirectory> " ++ dir
  runIO $ D.setCurrentDirectory dir

pwd :: IO String
pwd =  D.getCurrentDirectory

renameMsg :: String -> String -> String -> String
renameMsg tag src dst = unwords ["<" ++ tag ++ "> ", src, "-->", dst]

renameDirectory :: String -> String -> Build ()
renameDirectory src dst = do
  traceCommand $ renameMsg "renameDirectory" src dst
  runIO $ D.renameDirectory src dst

renameFile :: String -> String -> Build ()
renameFile src dst = do
  traceCommand $ renameMsg "renameFile" src dst
  runIO $ D.renameFile src dst

confirmPath :: String -> Build ()
confirmPath path =
  readProcess ["ls", "-ld", path] >>= traceOut


unpackInDir :: FilePath -> FilePath -> Build ()
apath `unpackInDir` dir = do
  runIO . putStrLn $ unwords ["Unpacking", apath, "in", dir, "."]
  rawSystem ["tar", "-C", dir, "-zxf", apath]

unpack :: FilePath -> Build ()
unpack apath = apath `unpackInDir` takeDirectory apath

packInDir' :: FilePath -> FilePath -> FilePath -> Build ()
packInDir' pdir apath wdir = do
  runIO . putStrLn $ unwords ["Packing", pdir, "in", wdir, "into", apath, "."]
  rawSystem ["tar", "-C", wdir, "-zcf", apath, pdir]

packInDir :: FilePath -> FilePath -> Build ()
pdir `packInDir` wdir =
  packInDir' pdir (pdir <.> tarGz) wdir

cabalDebian :: Maybe String -> Build ()
cabalDebian mayRev =
  rawSystem [ "cabal-debian"
            , "--debianize" {- for cabal-debian 1.25 -}
            , "--quilt"
            , "--revision=" ++ fromMaybe "1~autogen1" mayRev
            ]


run :: String -> [String] -> Build ()
run cmd = rawSystem . (cmd :)

debuild :: [String] -> Build ()
debuild =  run "debuild"

data BuildMode = All | Bin

buildPackage :: BuildMode -> [String] -> Build ()
buildPackage mode opts = do
  let modeOpt All = []
      modeOpt Bin = ["-B"]
  debuild $ ["-uc", "-us"] ++ modeOpt mode ++ opts

rebuild :: BuildMode -> [String] -> Build ()
rebuild mode opts = do
  debuild ["clean"]
  buildPackage mode opts

reinstallPackages :: [String] -> Build ()
reinstallPackages pkgs {- Need to be shell escapes -} = do
  system $ unwords ["yes '' |", "sudo apt-get remove", unwords pkgs, "|| true"]
  rawSystem ["sudo", "debi"]

reinstallGhcLibrary :: BuildMode -> Hackage -> Build ()
reinstallGhcLibrary mode = reinstallPackages . pkgs mode where
  pkgs All = ghcLibraryBinPackages
  pkgs Bin = ghcLibraryPackages

withCurrentDir :: FilePath -> Build a -> Build a
withCurrentDir dir act = do
  saveDir <- runIO pwd
  chdir dir
  r <- act
  chdir saveDir
  return r

getBaseDir :: Build FilePath
getBaseDir =  runIO pwd >>= askBaseDir

withBaseCurrentDir :: Build a -> Build a
withBaseCurrentDir act = do
  baseDir <- getBaseDir
  withCurrentDir baseDir act

getBuildDir :: Build FilePath
getBuildDir =  runIO pwd >>= askBuildDir

withBuildDir :: (FilePath -> Build a) -> Build a
withBuildDir f = getBuildDir >>= f
