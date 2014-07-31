module Debian.Package.Build.Cabal
       ( findDescriptionFile
       , parsePackageDescription
       , hackageLongName, hackageName, hackageVersion

       , setupCmd, clean, sdist
       , configure, build, install, register
       )  where

import Control.Applicative ((<$>))
import Control.Monad (filterM)
import Control.Monad.Trans.Class (lift)
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

import Debian.Package.Build.Monad (Trace, traceCommand)


-- | Find .cabal file
findDescriptionFile :: FilePath -> IO (Maybe FilePath)
findDescriptionFile dir = do
  fs  <-  getDirectoryContents dir
  let find f
        | length f > length suf  &&
          suf `isSuffixOf` f           =  doesFileExist $ dir </> f
        | otherwise                    =  return False
        where suf = ".cabal"
  fmap (dir </>) . listToMaybe <$> filterM find fs

-- | Parse .cabal file
parsePackageDescription :: FilePath -> IO PackageDescription
parsePackageDescription =  (packageDescription `fmap`) . readPackageDescription silent

-- | Hackage name and version string from 'PackageDescription'
hackageLongName :: PackageDescription -> String
hackageLongName =  show . disp . package

-- | Hackage name string from 'PackageDescription'
hackageName :: PackageDescription -> String
hackageName =  show . disp . pkgName . package

-- | Hackage version string from 'PackageDescription'
hackageVersion :: PackageDescription -> String
hackageVersion =  show . disp . pkgVersion . package

_testDotCabal :: IO PackageDescription
_testDotCabal =  do Just path <- findDescriptionFile "."
                    parsePackageDescription path

setup :: [String] -> Trace ()
setup args =  do
  traceCommand (unwords $ "<cabal>" : args)
  lift $ args `withArgs` defaultMain

-- | Call cabal library defaultMain like Setup.hs
setupCmd :: String -> [String] -> Trace ()
setupCmd cmd = setup . (cmd : )

-- | Cabal library defaultMain with sub-command clean
clean :: [String] -> Trace ()
clean =  setupCmd "clean"

-- | Cabal library defaultMain with sub-command configure
configure :: [String] -> Trace ()
configure =  setupCmd "configure"

-- | Cabal library defaultMain with sub-command sdist
sdist :: [String] -> Trace ()
sdist =  setupCmd "sdist"

-- | Cabal library defaultMain with sub-command build
build :: [String] -> Trace ()
build =  setupCmd "build"

-- | Cabal library defaultMain with sub-command install
install :: [String] -> Trace ()
install =  setupCmd "install"

-- | Cabal library defaultMain with sub-command register
register :: [String] -> Trace ()
register =  setupCmd "register"
