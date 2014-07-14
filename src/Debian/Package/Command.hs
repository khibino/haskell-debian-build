
module Debian.Package.Command (
  chdir, pwd,
  renameDirectory, renameFile,

  confirmPath,

  unpackInDir, unpack, packInDir', packInDir,

  cabalDebian,

  debuild,

  BuildMode (..),

  build, rebuild,

  reinstallPackages,

  reinstallGhcLibrary
  ) where

import Data.Maybe (fromMaybe)

import System.FilePath ((<.>), takeDirectory)
import qualified System.Directory as D

import Debian.Package.Internal
  (tarGz, readProcess', rawSystem', system', traceCommand, traceOut)
import Debian.Package.Hackage (Hackage, ghcLibraryBinPackages, ghcLibraryPackages)


chdir :: String -> IO ()
chdir dir =  do
  traceCommand $ "<setCurrentDirectory> " ++ dir
  D.setCurrentDirectory dir

pwd :: IO String
pwd =  D.getCurrentDirectory

renameMsg :: String -> String -> String -> String
renameMsg tag src dst = unwords ["<" ++ tag ++ "> ", src, "-->", dst]

renameDirectory :: String -> String -> IO ()
renameDirectory src dst = do
  traceCommand $ renameMsg "renameDirectory" src dst
  D.renameDirectory src dst

renameFile :: String -> String -> IO ()
renameFile src dst = do
  traceCommand $ renameMsg "renameFile" src dst
  D.renameFile src dst

confirmPath :: String -> IO ()
confirmPath path =
  readProcess' ["ls", "-ld", path] >>= traceOut


unpackInDir :: FilePath -> FilePath -> IO ()
apath `unpackInDir` dir = do
  putStrLn $ unwords ["Unpacking", apath, "in", dir, "."]
  rawSystem' ["tar", "-C", dir, "-zxf", apath]

unpack :: FilePath -> IO ()
unpack apath = apath `unpackInDir` takeDirectory apath

packInDir' :: FilePath -> FilePath -> FilePath -> IO ()
packInDir' pdir apath wdir = do
  putStrLn $ unwords ["Packing", pdir, "in", wdir, "into", apath, "."]
  rawSystem' ["tar", "-C", wdir, "-zcf", apath, pdir]

packInDir :: FilePath -> FilePath -> IO ()
pdir `packInDir` wdir =
  packInDir' pdir (pdir <.> tarGz) wdir

cabalDebian :: Maybe String -> IO ()
cabalDebian mayRev = do
  rawSystem' [ "cabal-debian"
             , "--debianize" {- for cabal-debian 1.25 -}
             , "--quilt"
             , "--revision=" ++ fromMaybe "1~autogen1" mayRev
             ]


run :: String -> [String] -> IO ()
run cmd = rawSystem' . (cmd :)

debuild :: [String] -> IO ()
debuild =  run "debuild"

data BuildMode = All | Bin

build :: BuildMode -> [String] -> IO ()
build mode opts = do
  let modeOpt All = []
      modeOpt Bin = ["-B"]
  debuild $ ["-uc", "-us"] ++ modeOpt mode ++ opts

rebuild :: BuildMode -> [String] -> IO ()
rebuild mode opts = do
  debuild ["clean"]
  build mode opts

reinstallPackages :: [String] -> IO ()
reinstallPackages pkgs = do
  system' $ unwords ["yes '' |", "sudo apt-get remove", unwords pkgs, "|| true"]
  rawSystem' ["sudo", "debi"]

reinstallGhcLibrary :: BuildMode -> Hackage -> IO ()
reinstallGhcLibrary mode = reinstallPackages . pkgs mode where
  pkgs All = ghcLibraryBinPackages
  pkgs Bin = ghcLibraryPackages
