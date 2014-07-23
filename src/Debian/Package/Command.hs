
module Debian.Package.Command
       ( chdir, pwd

       , createDirectoryIfMissing, renameDirectory, renameFile

       , confirmPath

       , unpackInDir, unpack, packInDir', packInDir

       , cabalDebian', cabalDebian

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

import Debian.Package.Internal (tarGz)
import Debian.Package.Hackage (Hackage, ghcLibraryBinPackages, ghcLibraryPackages)
import Debian.Package.Monad (Trace, traceCommand, traceOut)


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

createDirectoryIfMissing :: String -> Trace ()
createDirectoryIfMissing dir = do
  traceCommand $ "<createDirectoryIfMissing True> " ++ dir
  lift $ D.createDirectoryIfMissing True dir

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


withCurrentDir' :: FilePath -> Trace a -> Trace a
withCurrentDir' dir act = do
  saveDir <- lift pwd
  chdir dir
  r <- act
  chdir saveDir
  return r

cabalDebian' :: Maybe String -> Trace ()
cabalDebian' mayRev =
  rawSystem'
  [ "cabal-debian"
  , "--debianize" {- for cabal-debian 1.25 -}
  , "--quilt"
  , "--revision=" ++ fromMaybe "1~autogen1" mayRev
  ]

cabalDebian :: FilePath -> Maybe String -> Trace ()
cabalDebian dir = withCurrentDir' dir . cabalDebian'


run :: String -> [String] -> Trace ()
run cmd = rawSystem' . (cmd :)

debuild' :: [String] -> Trace ()
debuild' =  run "debuild"

debuild :: FilePath -> [String] -> Trace ()
debuild dir = withCurrentDir' dir . debuild'

data BuildMode = All | Bin

buildPackage :: FilePath -> BuildMode -> [String] -> Trace ()
buildPackage dir mode opts = do
  let modeOpt All = []
      modeOpt Bin = ["-B"]
  debuild dir $ ["-uc", "-us"] ++ modeOpt mode ++ opts

rebuild :: FilePath -> BuildMode -> [String] -> Trace ()
rebuild dir mode opts = do
  debuild dir ["clean"]
  buildPackage dir mode opts

reinstallPackages :: FilePath -> [String] -> Trace ()
reinstallPackages dir pkgs {- Need to be shell escapes -} = withCurrentDir' dir $ do
  system' $ unwords ["yes '' |", "sudo apt-get remove", unwords pkgs, "|| true"]
  rawSystem' ["sudo", "debi"]

reinstallGhcLibrary :: FilePath -> BuildMode -> Hackage -> Trace ()
reinstallGhcLibrary dir mode = reinstallPackages dir . pkgs mode where
  pkgs All = ghcLibraryBinPackages
  pkgs Bin = ghcLibraryPackages
