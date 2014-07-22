
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

import Debian.Package.Internal (tarGz, traceCommandIO, traceOutIO)
import Debian.Package.Hackage (Hackage, ghcLibraryBinPackages, ghcLibraryPackages)
import Debian.Package.Build.Monad
  (Trace, traceIO, Build, runIO, liftTrace, askBaseDir, askBuildDir)


traceCommand :: String -> Trace ()
traceCommand =  traceIO . traceCommandIO

traceOut :: String -> Trace ()
traceOut =  traceIO . traceOutIO

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

-- readProcess :: [String] -> Build String
-- readProcess =  liftTrace . readProcess'

rawSystem :: [String] -> Build ()
rawSystem =  liftTrace . rawSystem'

system :: String -> Build ()
system =  liftTrace . system'

chdir :: String -> Build ()
chdir dir =  do
  liftTrace . traceCommand $ "<setCurrentDirectory> " ++ dir
  runIO $ D.setCurrentDirectory dir

pwd :: IO String
pwd =  D.getCurrentDirectory

renameMsg :: String -> String -> String -> String
renameMsg tag src dst = unwords ["<" ++ tag ++ "> ", src, "-->", dst]

renameDirectory :: String -> String -> Build ()
renameDirectory src dst = do
  liftTrace . traceCommand $ renameMsg "renameDirectory" src dst
  runIO $ D.renameDirectory src dst

renameFile :: String -> String -> Build ()
renameFile src dst = do
  liftTrace . traceCommand $ renameMsg "renameFile" src dst
  runIO $ D.renameFile src dst

confirmPath :: String -> Trace ()
confirmPath path =
  readProcess' ["ls", "-ld", path] >>= traceOut


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
