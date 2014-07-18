module Debian.Package.Build.Cabal (
  findDescriptionFile,
  parsePackageDescription,
  hackageLongName,
  hackageName, hackageVersion,

  setupCmd,
  clean, sdist,
  configure, build, install, register
  )  where

import Control.Applicative ((<$>))
import Control.Monad (filterM)
import Data.Maybe (listToMaybe)
import Data.List (isSuffixOf)
import System.FilePath ((</>))
import System.Directory (getDirectoryContents, doesFileExist)
import System.Environment (withArgs)

import Distribution.Text (disp)
import Distribution.Verbosity (silent)
import Distribution.Package (pkgName, pkgVersion)
import Distribution.PackageDescription
  (GenericPackageDescription, packageDescription, PackageDescription, package)
import Distribution.PackageDescription.Parse (readPackageDescription)

import Distribution.Simple (defaultMain)

import Debian.Package.Internal (traceCommandIO)

findDescriptionFile :: FilePath -> IO (Maybe FilePath)
findDescriptionFile dir = do
  fs  <-  getDirectoryContents dir
  let find f
        | length f > length suf  &&
          suf `isSuffixOf` f           =  doesFileExist $ dir </> f
        | otherwise                    =  return False
        where suf = ".cabal"
  fmap (dir </>) . listToMaybe <$> filterM find fs

parsePackageDescription :: FilePath -> IO PackageDescription
parsePackageDescription =  (packageDescription `fmap`) . readPackageDescription silent

hackageLongName :: PackageDescription -> String
hackageLongName =  show . disp . package

hackageName :: PackageDescription -> String
hackageName =  show . disp . pkgName . package

hackageVersion :: PackageDescription -> String
hackageVersion =  show . disp . pkgVersion . package

_testDotCabal :: IO PackageDescription
_testDotCabal =  do Just path <- findDescriptionFile "."
                    parsePackageDescription path

setup :: [String] -> IO ()
setup args =  do
  traceCommandIO (unwords $ "<cabal>" : args)
  args `withArgs` defaultMain

setupCmd :: String -> [String] -> IO ()
setupCmd cmd = setup . (cmd : )

clean :: [String] -> IO ()
clean =  setupCmd "clean"

configure :: [String] -> IO ()
configure =  setupCmd "configure"

sdist :: [String] -> IO ()
sdist =  setupCmd "sdist"

build :: [String] -> IO ()
build =  setupCmd "build"

install :: [String] -> IO ()
install =  setupCmd "install"

register :: [String] -> IO ()
register =  setupCmd "register"
