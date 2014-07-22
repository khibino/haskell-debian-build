{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Debian.Package.Build.Run (
  origArchiveName, nativeArchive,

  removeBuildDir,

  copyDebianDir, cabalDebianDir,

  rsyncGenOrigSources,
  rsyncGenNativeSources,
  rsyncGenSources,

  cabalGenOrigSources,
  cabalGenSources,
  cabalAutogenSources,

  genSources
  ) where

import System.FilePath ((</>), takeFileName, takeDirectory, takeBaseName)
import System.Directory
  (createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import Control.Applicative ((<$>), (<|>))
import Control.Monad (when)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import Data.Maybe (fromMaybe)
import Data.List (isPrefixOf)

import Debian.Package.Hackage (Hackage, hackageLongName, hackageArchive)
import Debian.Package.Source
  (Package, origArchiveName, nativeArchiveName, sourceDirName, isNative,
   HaskellPackage, hackage, package, parsePackageFromChangeLog, haskellPackageFromPackage)
import Debian.Package.Build.Monad (Build, runIO, Config (..), askConfig)
import Debian.Package.Build.Command
  (confirmPath, renameFile, renameDirectory, unpack, packInDir', cabalDebian, withCurrentDir,
   getBaseDir, withBaseCurrentDir, getBuildDir, withBuildDir, rawSystem')
import qualified Debian.Package.Build.Cabal as Cabal


removeBuildDir :: Build ()
removeBuildDir = do
  bldDir <- getBuildDir
  runIO $ do
    found <- doesDirectoryExist bldDir
    when found $ rawSystem' ["rm", "-r", bldDir]

debianDirName :: Build FilePath
debianDirName =  do
  mayD <- mayDebianDirName <$> askConfig
  return $ fromMaybe "debian" mayD

origArchive :: Package -> Build FilePath
origArchive pkg =
  withBuildDir $ \w -> return $ w </> origArchiveName pkg

nativeArchive :: Package -> Build FilePath
nativeArchive pkg =
  withBuildDir $ \w -> return $ w </> nativeArchiveName pkg

sourceDir :: Package -> Build FilePath
sourceDir pkg =
  withBuildDir $ \w -> return $ w </> sourceDirName pkg

copyDebianDir :: FilePath -> Build ()
copyDebianDir srcDir = do
  debDN       <- debianDirName
  baseDir     <- getBaseDir
  runIO $ rawSystem' ["cp", "-a", baseDir </> debDN, srcDir </> "."]

cabalDebianDir :: Maybe String -> FilePath -> Build ()
cabalDebianDir mayRev srcDir =
  withCurrentDir srcDir $ cabalDebian mayRev


rsyncGenOrigSourceDir :: Package -> Build FilePath
rsyncGenOrigSourceDir pkg = do
  srcDir   <- sourceDir pkg
  debDN    <- debianDirName
  baseDir  <- getBaseDir
  bldDir   <- getBuildDir
  let excludes = [takeFileName d
                 | d <- [bldDir]
                 , baseDir `isPrefixOf` d ]
                 ++ [debDN]
  runIO $ do
    createDirectoryIfMissing True srcDir
    rawSystem'
      $  ["rsync", "-auv"]
      ++ ["--exclude=" ++ e | e <- excludes]
      ++ [baseDir </> ".", srcDir </> "." ]
  return srcDir

rsyncGenOrigSources :: Package -> Build (FilePath, FilePath)
rsyncGenOrigSources pkg = do
  srcDir <- rsyncGenOrigSourceDir pkg
  origPath  <- origArchive pkg
  withBuildDir $ packInDir' (takeFileName srcDir) origPath
  copyDebianDir srcDir
  confirmPath srcDir
  return (origPath, srcDir)

rsyncGenNativeSources :: Package -> Build (FilePath, FilePath)
rsyncGenNativeSources pkg = do
  srcDir <- rsyncGenOrigSourceDir pkg
  copyDebianDir srcDir
  nativePath <- nativeArchive pkg
  withBuildDir $ packInDir' (takeFileName srcDir) nativePath
  confirmPath srcDir
  return (nativePath, srcDir)

rsyncGenSources :: Package -> Build (FilePath, FilePath)
rsyncGenSources pkg
  | isNative pkg = rsyncGenNativeSources pkg
  | otherwise    = rsyncGenOrigSources   pkg


cabalGenArchive :: Hackage -> Build FilePath
cabalGenArchive hkg = do
  withBaseCurrentDir . runIO $ Cabal.sdist []
  baseDir <- getBaseDir
  let apath = baseDir </> hackageArchive hkg
  confirmPath apath
  return apath

cabalGenOrigArchive :: HaskellPackage -> Build FilePath
cabalGenOrigArchive hpkg = do
  origPath <- origArchive $ package hpkg
  apath    <- cabalGenArchive $ hackage hpkg
  runIO $ createDirectoryIfMissing True $ takeDirectory origPath
  renameFile apath origPath
  return origPath

cabalGenOrigSources :: HaskellPackage -> Build (FilePath, FilePath)
cabalGenOrigSources hpkg = do
  origPath <- cabalGenOrigArchive hpkg
  srcDir   <- sourceDir $ package hpkg
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
  exist <- runIO $ doesDirectoryExist tmpDD
  when exist (fail $ "Invalid state: directory already exist: " ++ tmpDD)

  cabalDebianDir Nothing baseDir

  debDir   <-  (</> ddName) <$> getBuildDir
  runIO $ createDirectoryIfMissing True $ takeDirectory debDir
  renameDirectory tmpDD debDir
  return debDir

cabalAutogenSources :: String -> Build (FilePath, FilePath)
cabalAutogenSources hname = do
  debDir   <-  cabalAutogenDebianDir
  pkg      <-  runIO . parsePackageFromChangeLog $ debDir </> "changelog"
  hpkg     <-  either fail return $ haskellPackageFromPackage hname pkg
  pair@(_, srcDir)  <-  cabalGenOrigSources hpkg
  renameDirectory debDir (srcDir </> takeFileName debDir)
  return pair


findDebianChangeLog :: MaybeT Build FilePath
findDebianChangeLog =  MaybeT $ do
  baseDir  <-  getBaseDir
  debDN    <-  debianDirName
  let changelog = baseDir </> debDN </> "changelog"
  runIO $ do
    exist <- doesFileExist changelog
    return $ if exist
             then Just changelog
             else Nothing

findCabalDescription :: MaybeT Build FilePath
findCabalDescription =  MaybeT (getBaseDir >>= runIO . Cabal.findDescriptionFile)

genSources :: Build (Maybe (FilePath, FilePath))
genSources =  runMaybeT $
  do clog <- findDebianChangeLog
     pkg  <- lift . runIO $ parsePackageFromChangeLog clog
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
