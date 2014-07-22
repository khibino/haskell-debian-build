
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
  getBuildDir, withBuildDir,

  readProcess', rawSystem', system'
  ) where

import Data.Maybe (fromMaybe)
import Control.Arrow ((&&&))
import Control.Monad.Trans.Class (lift)
import System.FilePath ((<.>), takeDirectory)
import qualified System.Directory as D
import qualified System.Process as Process
import System.Exit (ExitCode (..))

import Debian.Package.Internal (tarGz)
import Debian.Package.Hackage (Hackage, ghcLibraryBinPackages, ghcLibraryPackages)
import Debian.Package.Build.Monad
  (Trace, traceCommand, traceOut, Build, runIO, liftTrace, askBaseDir, askBuildDir)


splitCommand :: [a] -> (a, [a])
splitCommand =  head &&& tail

handleExit :: String -> ExitCode -> IO ()
handleExit cmd = d  where
  d (ExitFailure rv) = fail $ unwords ["Failed with", show rv ++ ":", cmd]
  d  ExitSuccess     = return ()

readProcess' :: [String] -> Trace String
readProcess' cmd0 = do
  traceCommand $ unwords cmd0
  lift $ do
    let (cmd, args) = splitCommand cmd0
    Process.readProcess cmd args ""

rawSystem' :: [String] -> Trace ()
rawSystem' cmd0 = do
  traceCommand $ unwords cmd0
  lift $ do
    let (cmd, args) = splitCommand cmd0
    Process.rawSystem cmd args >>= handleExit cmd

system' :: String -> Trace ()
system' cmd = do
  traceCommand cmd
  lift $ Process.system cmd >>= handleExit cmd

chdir :: String -> Trace ()
chdir dir =  do
  traceCommand $ "<setCurrentDirectory> " ++ dir
  lift $ D.setCurrentDirectory dir

pwd :: IO String
pwd =  D.getCurrentDirectory

renameMsg :: String -> String -> String -> String
renameMsg tag src dst = unwords ["<" ++ tag ++ "> ", src, "-->", dst]

renameDirectory :: String -> String -> Trace ()
renameDirectory src dst = do
  traceCommand $ renameMsg "renameDirectory" src dst
  lift $ D.renameDirectory src dst

renameFile :: String -> String -> Trace ()
renameFile src dst = do
  traceCommand $ renameMsg "renameFile" src dst
  lift $ D.renameFile src dst

confirmPath :: String -> Trace ()
confirmPath path =
  readProcess' ["ls", "-ld", path] >>= traceOut


unpackInDir :: FilePath -> FilePath -> Trace ()
apath `unpackInDir` dir = do
  lift . putStrLn $ unwords ["Unpacking", apath, "in", dir, "."]
  rawSystem' ["tar", "-C", dir, "-zxf", apath]

unpack :: FilePath -> Trace ()
unpack apath = apath `unpackInDir` takeDirectory apath

packInDir' :: FilePath -> FilePath -> FilePath -> Trace ()
packInDir' pdir apath wdir = do
  lift . putStrLn $ unwords ["Packing", pdir, "in", wdir, "into", apath, "."]
  rawSystem' ["tar", "-C", wdir, "-zcf", apath, pdir]

packInDir :: FilePath -> FilePath -> Trace ()
pdir `packInDir` wdir =
  packInDir' pdir (pdir <.> tarGz) wdir

cabalDebian :: Maybe String -> Trace ()
cabalDebian mayRev =
  rawSystem'
  [ "cabal-debian"
  , "--debianize" {- for cabal-debian 1.25 -}
  , "--quilt"
  , "--revision=" ++ fromMaybe "1~autogen1" mayRev
  ]


run :: String -> [String] -> Trace ()
run cmd = rawSystem' . (cmd :)

debuild :: [String] -> Trace ()
debuild =  run "debuild"

data BuildMode = All | Bin

buildPackage :: BuildMode -> [String] -> Trace ()
buildPackage mode opts = do
  let modeOpt All = []
      modeOpt Bin = ["-B"]
  debuild $ ["-uc", "-us"] ++ modeOpt mode ++ opts

rebuild :: BuildMode -> [String] -> Trace ()
rebuild mode opts = do
  debuild ["clean"]
  buildPackage mode opts

reinstallPackages :: [String] -> Trace ()
reinstallPackages pkgs {- Need to be shell escapes -} = do
  system' $ unwords ["yes '' |", "sudo apt-get remove", unwords pkgs, "|| true"]
  rawSystem' ["sudo", "debi"]

reinstallGhcLibrary :: BuildMode -> Hackage -> Trace ()
reinstallGhcLibrary mode = reinstallPackages . pkgs mode where
  pkgs All = ghcLibraryBinPackages
  pkgs Bin = ghcLibraryPackages

withCurrentDir :: FilePath -> Build a -> Build a
withCurrentDir dir act = do
  saveDir <- runIO pwd
  liftTrace $ chdir dir
  r <- act
  liftTrace $ chdir saveDir
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
