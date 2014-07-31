{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Debian.Package.Build
       ( origArchive, nativeArchive

       , withCurrentDir, withBaseCurrentDir

       , removeBuildDir

       , copyDebianDir

       , rsyncGenOrigSources, rsyncGenNativeSources, rsyncGenSources

       , cabalGenOrigSources, cabalGenSources, cabalAutogenSources

       , genSources
       ) where

import System.FilePath ((</>), takeFileName, takeDirectory, takeBaseName)
import System.Directory
  (doesDirectoryExist, doesFileExist)
import Control.Applicative ((<$>), (<|>))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import Data.Maybe (fromMaybe)
import Data.List (isPrefixOf)

import Debian.Package.Hackage (Hackage, hackageLongName, hackageArchive)
import Debian.Package.Source
  (Source, origArchiveName, nativeArchiveName, sourceDirName, isNative,
   HaskellPackage, hackage, package, haskellPackageFromPackage)
import Debian.Package.Monad
  (Build, liftTrace, Config (..), askConfig, askBaseDir, askBuildDir)
import Debian.Package.Command
  (chdir, pwd, createDirectoryIfMissing, confirmPath, renameFile, renameDirectory,
   unpack, packInDir', cabalDebian, dpkgParseChangeLog, rawSystem')
import qualified Debian.Package.Cabal as Cabal


withCurrentDir :: FilePath -> Build a -> Build a
withCurrentDir dir act = do
  saveDir <- liftIO pwd
  liftTrace $ chdir dir
  r <- act
  liftTrace $ chdir saveDir
  return r

getBaseDir :: Build FilePath
getBaseDir =  liftIO pwd >>= askBaseDir

withBaseCurrentDir :: Build a -> Build a
withBaseCurrentDir act = do
  baseDir <- getBaseDir
  withCurrentDir baseDir act

getBuildDir :: Build FilePath
getBuildDir =  liftIO pwd >>= askBuildDir

withBuildDir :: (FilePath -> Build a) -> Build a
withBuildDir f = getBuildDir >>= f

removeBuildDir :: Build ()
removeBuildDir = do
  bldDir <- getBuildDir
  liftTrace $ do
    found <- liftIO $ doesDirectoryExist bldDir
    when found $ rawSystem' ["rm", "-r", bldDir]

debianDirName :: Build FilePath
debianDirName =  do
  mayD <- mayDebianDirName <$> askConfig
  return $ fromMaybe "debian" mayD

origArchive :: Source -> Build FilePath
origArchive pkg =
  withBuildDir $ \w -> return $ w </> origArchiveName pkg

nativeArchive :: Source -> Build FilePath
nativeArchive pkg =
  withBuildDir $ \w -> return $ w </> nativeArchiveName pkg

sourceDir :: Source -> Build FilePath
sourceDir pkg =
  withBuildDir $ \w -> return $ w </> sourceDirName pkg

copyDebianDir :: FilePath -> Build ()
copyDebianDir srcDir = do
  debDN       <- debianDirName
  baseDir     <- getBaseDir
  liftTrace $ rawSystem' ["cp", "-a", baseDir </> debDN, srcDir </> "."]


rsyncGenOrigSourceDir :: Source -> Build FilePath
rsyncGenOrigSourceDir pkg = do
  srcDir   <- sourceDir pkg
  debDN    <- debianDirName
  baseDir  <- getBaseDir
  bldDir   <- getBuildDir
  let excludes = [takeFileName d
                 | d <- [bldDir]
                 , baseDir `isPrefixOf` d ]
                 ++ [debDN]
  liftTrace $ do
    createDirectoryIfMissing srcDir
    rawSystem'
      $  ["rsync", "-auv"]
      ++ ["--exclude=" ++ e | e <- excludes]
      ++ [baseDir </> ".", srcDir </> "." ]
  return srcDir

rsyncGenOrigSources :: Source -> Build (FilePath, FilePath)
rsyncGenOrigSources pkg = do
  srcDir <- rsyncGenOrigSourceDir pkg
  origPath  <- origArchive pkg
  withBuildDir $ liftTrace . packInDir' (takeFileName srcDir) origPath
  copyDebianDir srcDir
  liftTrace $ confirmPath srcDir
  return (origPath, srcDir)

rsyncGenNativeSources :: Source -> Build (FilePath, FilePath)
rsyncGenNativeSources pkg = do
  srcDir <- rsyncGenOrigSourceDir pkg
  copyDebianDir srcDir
  nativePath <- nativeArchive pkg
  withBuildDir $ liftTrace . packInDir' (takeFileName srcDir) nativePath
  liftTrace $ confirmPath srcDir
  return (nativePath, srcDir)

rsyncGenSources :: Source -> Build (FilePath, FilePath)
rsyncGenSources pkg
  | isNative pkg = rsyncGenNativeSources pkg
  | otherwise    = rsyncGenOrigSources   pkg


cabalGenArchive :: Hackage -> Build FilePath
cabalGenArchive hkg = do
  withBaseCurrentDir . liftTrace $ Cabal.sdist []
  baseDir <- getBaseDir
  let apath = baseDir </> hackageArchive hkg
  liftTrace $ confirmPath apath
  return apath

cabalGenOrigArchive :: HaskellPackage -> Build FilePath
cabalGenOrigArchive hpkg = do
  origPath <- origArchive $ package hpkg
  apath    <- cabalGenArchive $ hackage hpkg
  liftTrace $ do
    createDirectoryIfMissing $ takeDirectory origPath
    renameFile apath origPath
  return origPath

cabalGenOrigSources :: HaskellPackage -> Build (FilePath, FilePath)
cabalGenOrigSources hpkg = do
  origPath <- cabalGenOrigArchive hpkg
  srcDir   <- sourceDir $ package hpkg
  liftTrace $ do
    unpack origPath
    renameDirectory
      (takeDirectory origPath </> hackageLongName (hackage hpkg))
      srcDir
    confirmPath srcDir
  return (origPath, srcDir)

cabalGenSources :: HaskellPackage -> Build (FilePath, FilePath)
cabalGenSources hpkg = do
  pair@(_, srcDir) <- cabalGenOrigSources hpkg
  copyDebianDir srcDir
  return pair

cabalAutogenDebianDir :: Build FilePath
cabalAutogenDebianDir = do
  baseDir  <-  getBaseDir
  let ddName =  "debian"
      tmpDD  =  baseDir </> ddName
  exist <- liftIO $ doesDirectoryExist tmpDD
  when exist (fail $ "Invalid state: directory already exist: " ++ tmpDD)

  debDir   <-  (</> ddName) <$> getBuildDir
  liftTrace $ do
    cabalDebian baseDir Nothing
    createDirectoryIfMissing $ takeDirectory debDir
    renameDirectory tmpDD debDir
  return debDir

cabalAutogenSources :: String -> Build (FilePath, FilePath)
cabalAutogenSources hname = do
  debDir   <-  cabalAutogenDebianDir
  pkg      <-  liftTrace . dpkgParseChangeLog $ debDir </> "changelog"
  hpkg     <-  either fail return $ haskellPackageFromPackage hname pkg
  pair@(_, srcDir)  <-  cabalGenOrigSources hpkg
  liftTrace $ renameDirectory debDir (srcDir </> takeFileName debDir)
  return pair


findDebianChangeLog :: MaybeT Build FilePath
findDebianChangeLog =  MaybeT $ do
  baseDir  <-  getBaseDir
  debDN    <-  debianDirName
  let changelog = baseDir </> debDN </> "changelog"
  liftIO $ do
    exist <- doesFileExist changelog
    return $ if exist
             then Just changelog
             else Nothing

findCabalDescription :: MaybeT Build FilePath
findCabalDescription =  MaybeT (getBaseDir >>= liftIO . Cabal.findDescriptionFile)

genSources :: Build (Maybe (FilePath, FilePath))
genSources =  runMaybeT $
  do clog <- findDebianChangeLog
     pkg  <- lift . liftTrace $ dpkgParseChangeLog clog
     (do hname <- takeBaseName <$> findCabalDescription
         hpkg  <- either fail return $ haskellPackageFromPackage hname pkg
         lift $ cabalGenSources hpkg
      <|>
      do lift $ rsyncGenSources pkg)
  <|>
  do hname <- takeBaseName <$> findCabalDescription
     lift $ cabalAutogenSources hname
  <|>
  do fail "No source generate rule found."
