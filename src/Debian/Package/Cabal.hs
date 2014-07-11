module Debian.Package.Cabal (
  findDescriptionFile,
  parsePackageDescription,
  hackageLongName,
  hackageName, hackageVersion,

  -- setup,
  setupCmd,
  clean, sdist,
  configure, build, install, register
  )  where

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

import Debian.Package.Internal (traceCommand)

cabalSuffix :: String
cabalSuffix =  ".cabal"

findDescriptionFile :: FilePath -> IO (Maybe FilePath)
findDescriptionFile dir =
  do ps <- getDirectoryContents dir
     loop ps
  where check path
          | cabalSuffix `isSuffixOf` path &&
            length path > length cabalSuffix = doesFileExist path
          | otherwise                        = return False
        loop (p:ps) = do fp <- check p
                         if fp
                           then return (Just $ dir </> p)
                           else loop ps
        loop []     =           return Nothing

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
  traceCommand (unwords $ "<cabal>" : args)
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
