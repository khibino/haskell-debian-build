-- |
-- Module      : Debian.Package.Build.Command
-- Copyright   : 2014 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- This module provides trace-able action instances like commands.
module Debian.Package.Build.Command
       ( chdir, pwd

       , createDirectoryIfMissing, renameDirectory, renameFile

       , confirmPath

       , unpackInDir, unpack, packInDir', packInDir

       , cabalDebian', cabalDebian, packageVersion, dpkgParseChangeLog

       , debuild, debi

       , BuildMode (..)

       , buildPackage, rebuild

       , removeGhcLibrary

       , withCurrentDir'

       , readProcess', rawSystem', system'
       ) where

import Data.Maybe (fromMaybe)
import Control.Arrow ((&&&))
import Control.Applicative ((<$>))
import Control.Monad.Trans.Class (lift)
import System.FilePath ((<.>), takeDirectory)
import qualified System.Directory as D
import qualified System.Process as Process
import System.Exit (ExitCode (..))
import Data.Version (Version, versionBranch, showVersion)

import Debian.Package.Data (Hackage, ghcLibraryBinPackages, ghcLibraryPackages, Source, parseChangeLog, DebianVersion, readDebianVersion, origVersion')
import Debian.Package.Build.Monad (Trace, traceCommand, traceOut, putLog, bracketTrace_)


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
  putLog $ unwords ["Unpacking", apath, "in", dir, "."]
  rawSystem' ["tar", "-C", dir, "-zxf", apath]

-- | Unpack .tar.gz under archive place.
unpack :: FilePath -> Trace ()
unpack apath = apath `unpackInDir` takeDirectory apath

-- | Pack directory into .tar.gz under working directory
packInDir' :: FilePath -> FilePath -> FilePath -> Trace ()
packInDir' pdir apath wdir = do
  putLog $ unwords ["Packing", pdir, "in", wdir, "into", apath, "."]
  rawSystem' ["tar", "-C", wdir, "-zcf", apath, pdir]

-- | Pack directory into same location .tar.gz under working directory
packInDir :: FilePath -> FilePath -> Trace ()
pdir `packInDir` wdir =
  packInDir' pdir (pdir <.> "tar" <.> "gz") wdir


-- | Run action under specified directory
withCurrentDir' :: FilePath -> Trace a -> Trace a
withCurrentDir' dir act = do
  saveDir <- lift pwd
  bracketTrace_
    (chdir dir)
    (chdir saveDir)
    act

-- | Just call /cabal-debian/ command
cabalDebian' :: Maybe String -> Trace ()
cabalDebian' mayRev = do
  ver <-  origVersion' <$> packageVersion "cabal-debian"
  case versionBranch ver of
    (x:_) | x <= 1     ->  fail $ "Version of cabal-debian is TOO OLD: " ++ showVersion ver ++
                           " - Under version 1 generates wrong dependencies."
          | otherwise  ->  return ()
    []                 ->  return ()

  rawSystem'
    [ "cabal-debian"
    , "--quilt"
    , "--revision=" ++ fromMaybe "1~autogen1" mayRev
    ]

-- | Call /cabal-debian/ command under specified directory
cabalDebian :: FilePath -> Maybe String -> Trace ()
cabalDebian dir = withCurrentDir' dir . cabalDebian'

-- | Query debian package version
packageVersion :: String -> Trace DebianVersion
packageVersion pkg = do
  vstr <- readProcess' ["dpkg-query", "--show", "--showformat=${Version}", pkg]
  maybe (fail $ "readDebianVersion: failed: " ++ vstr) return
    $ readDebianVersion vstr

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

-- | Install packages under specified source package directory
debi :: FilePath -> [String] -> Trace ()
debi dir = withCurrentDir' dir . rawSystem' . (["sudo", "debi"] ++)

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

-- | Remove ghc library packages under specified source package directory
removeGhcLibrary :: BuildMode -> Hackage -> Trace ()
removeGhcLibrary mode hkg = do
  let pkgs All = ghcLibraryBinPackages
      pkgs Bin = ghcLibraryPackages
  system' $ unwords ["yes '' |", "sudo apt-get remove", unwords $ pkgs mode hkg, "|| true"]
