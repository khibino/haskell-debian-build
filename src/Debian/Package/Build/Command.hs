
module Debian.Package.Build.Command
       ( chdir, pwd

       , createDirectoryIfMissing, renameDirectory, renameFile

       , confirmPath

       , unpackInDir, unpack, packInDir', packInDir

       , cabalDebian', cabalDebian, dpkgParseChangeLog

       , debuild

       , BuildMode (..)

       , buildPackage, rebuild

       , reinstallGhcLibrary

       , withCurrentDir'

       , readProcess', rawSystem', system'
       ) where

import Data.Maybe (fromMaybe)
import Control.Arrow ((&&&))
import Control.Monad.Trans.Class (lift)
import System.FilePath ((<.>), takeDirectory)
import qualified System.Directory as D
import qualified System.Process as Process
import System.Exit (ExitCode (..))

import Debian.Package.Data.Hackage (Hackage, ghcLibraryBinPackages, ghcLibraryPackages)
import Debian.Package.Data.Source (Source, parseChangeLog)
import Debian.Package.Build.Monad (Trace, traceCommand, traceOut)


splitCommand :: [a] -> (a, [a])
splitCommand =  head &&& tail

handleExit :: String -> ExitCode -> IO ()
handleExit cmd = d  where
  d (ExitFailure rv) = fail $ unwords ["Failed with", show rv ++ ":", cmd]
  d  ExitSuccess     = return ()

-- | Run command without shell and get standard output string.
readProcess' :: [String] -> Trace String
readProcess' cmd0 = do
  traceCommand $ unwords cmd0
  lift $ do
    let (cmd, args) = splitCommand cmd0
    Process.readProcess cmd args ""

-- | Run command without shell
rawSystem' :: [String] -> Trace ()
rawSystem' cmd0 = do
  traceCommand $ unwords cmd0
  lift $ do
    let (cmd, args) = splitCommand cmd0
    Process.rawSystem cmd args >>= handleExit cmd

-- | Run command with shell
system' :: String -> Trace ()
system' cmd = do
  traceCommand cmd
  lift $ Process.system cmd >>= handleExit cmd

-- | Change directory action
chdir :: String -> Trace ()
chdir dir =  do
  traceCommand $ "<setCurrentDirectory> " ++ dir
  lift $ D.setCurrentDirectory dir

-- | Action to get current working directory
pwd :: IO String
pwd =  D.getCurrentDirectory

-- | Create directory if missing
createDirectoryIfMissing :: String -> Trace ()
createDirectoryIfMissing dir = do
  traceCommand $ "<createDirectoryIfMissing True> " ++ dir
  lift $ D.createDirectoryIfMissing True dir

renameMsg :: String -> String -> String -> String
renameMsg tag src dst = unwords ["<" ++ tag ++ "> ", src, "-->", dst]

-- | Rename directory action. e.g. /renameDirectory from to/
renameDirectory :: String -> String -> Trace ()
renameDirectory src dst = do
  traceCommand $ renameMsg "renameDirectory" src dst
  lift $ D.renameDirectory src dst

-- | Rename file action. e.g. /renameFile from to/
renameFile :: String -> String -> Trace ()
renameFile src dst = do
  traceCommand $ renameMsg "renameFile" src dst
  lift $ D.renameFile src dst

-- | Confirm filepath using /ls/ command
confirmPath :: String -> Trace ()
confirmPath path =
  readProcess' ["ls", "-ld", path] >>= traceOut


-- | Unpack .tar.gz under directory.
unpackInDir :: FilePath -> FilePath -> Trace ()
apath `unpackInDir` dir = do
  lift . putStrLn $ unwords ["Unpacking", apath, "in", dir, "."]
  rawSystem' ["tar", "-C", dir, "-zxf", apath]

-- | Unpack .tar.gz under archive place.
unpack :: FilePath -> Trace ()
unpack apath = apath `unpackInDir` takeDirectory apath

-- | Pack directory into .tar.gz under working directory
packInDir' :: FilePath -> FilePath -> FilePath -> Trace ()
packInDir' pdir apath wdir = do
  lift . putStrLn $ unwords ["Packing", pdir, "in", wdir, "into", apath, "."]
  rawSystem' ["tar", "-C", wdir, "-zcf", apath, pdir]

-- | Pack directory into same location .tar.gz under working directory
packInDir :: FilePath -> FilePath -> Trace ()
pdir `packInDir` wdir =
  packInDir' pdir (pdir <.> "tar" <.> "gz") wdir


-- | Run action under specified directory
withCurrentDir' :: FilePath -> Trace a -> Trace a
withCurrentDir' dir act = do
  saveDir <- lift pwd
  chdir dir
  r <- act
  chdir saveDir
  return r

-- | Just call /cabal-debian/ command
cabalDebian' :: Maybe String -> Trace ()
cabalDebian' mayRev =
  rawSystem'
  [ "cabal-debian"
  , "--debianize" {- for cabal-debian 1.25 -}
  , "--quilt"
  , "--revision=" ++ fromMaybe "1~autogen1" mayRev
  ]

-- | Call /cabal-debian/ command under specified directory
cabalDebian :: FilePath -> Maybe String -> Trace ()
cabalDebian dir = withCurrentDir' dir . cabalDebian'

-- | Read debian changelog file and try to parse into 'Source'
dpkgParseChangeLog :: FilePath -> Trace Source
dpkgParseChangeLog cpath =  do
  str <- readProcess' ["dpkg-parsechangelog", "-l" ++ cpath]
  maybe (fail $ "parseChangeLog: failed: " ++ str) return
    $ parseChangeLog str


run :: String -> [String] -> Trace ()
run cmd = rawSystem' . (cmd :)

debuild' :: [String] -> Trace ()
debuild' =  run "debuild"

-- | Call /debuild/ under specified directory, with command line options
debuild :: FilePath -> [String] -> Trace ()
debuild dir = withCurrentDir' dir . debuild'

-- | Build mode, all or binary only
data BuildMode = All | Bin

-- | Build package using /debuild/ under specified directory
buildPackage :: FilePath -> BuildMode -> [String] -> Trace ()
buildPackage dir mode opts = do
  let modeOpt All = []
      modeOpt Bin = ["-B"]
  debuild dir $ ["-uc", "-us"] ++ modeOpt mode ++ opts

-- | Clean and build package using /debuild/ under specified directory
rebuild :: FilePath -> BuildMode -> [String] -> Trace ()
rebuild dir mode opts = do
  debuild dir ["clean"]
  buildPackage dir mode opts

reinstallPackages :: FilePath -> [String] -> Trace ()
reinstallPackages dir pkgs {- Need to be shell escapes -} = withCurrentDir' dir $ do
  system' $ unwords ["yes '' |", "sudo apt-get remove", unwords pkgs, "|| true"]
  rawSystem' ["sudo", "debi"]

-- | Re-install ghc library packages under specified source package directory
reinstallGhcLibrary :: FilePath -> BuildMode -> Hackage -> Trace ()
reinstallGhcLibrary dir mode = reinstallPackages dir . pkgs mode where
  pkgs All = ghcLibraryBinPackages
  pkgs Bin = ghcLibraryPackages
