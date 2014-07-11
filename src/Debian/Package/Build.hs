{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Debian.Package.Build (
  BaseDir, baseDirCurrent, baseDirSpecify,
  BuildDir, buildDirRelative, buildDirAbsolute,

  Config, buildDir,

  mayDebianDirName, defaultConfig,

  Build, runIO, runBuild, withCurrentDir,

  getBaseDir, withBaseCurrentDir,

  origArchiveName, nativeArchive,

  removeBuildDir,

  setupDebianDir,

  rsyncGenOrigSources,
  rsyncGenNativeSources,
  rsyncGenSources,

  cabalGenOrigSources
  ) where

import System.FilePath ((</>), takeFileName, takeDirectory)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.List (isPrefixOf)

import Debian.Package.Internal (rawSystem')
import Debian.Package.Hackage (Hackage, hackageLongName, hackageArchive)
import Debian.Package.Source
  (Package, origArchiveName, nativeArchiveName, sourceDirName, isNative,
   HaskellPackage, hackage, package)
import Debian.Package.Command
  (pwd, chdir, confirmPath, renameFile, renameDirectory, unpack, packInDir')
import qualified Debian.Package.Cabal as Cabal


newtype BaseDir = BaseDir { unBaseDir :: Maybe FilePath }

baseDirCurrent :: BaseDir
baseDirCurrent =  BaseDir Nothing

baseDirSpecify :: FilePath -> BaseDir
baseDirSpecify =  BaseDir . Just

newtype BuildDir = BuildDir (Either FilePath FilePath)

buildDirRelative :: FilePath -> BuildDir
buildDirRelative = BuildDir . Left

buildDirAbsolute :: FilePath -> BuildDir
buildDirAbsolute = BuildDir . Right

unBuildDir :: FilePath -> BuildDir -> FilePath
unBuildDir base (BuildDir b) = either (base </>) id b

instance Show BuildDir where
  show = d  where
    d (BuildDir (Left  p)) = "Relative " ++ p
    d (BuildDir (Right p)) = "Absolute " ++ p

data Config =
  Config
  { buildDir         :: BuildDir
  , mayDebianDirName :: Maybe FilePath
  } deriving Show

defaultConfig :: Config
defaultConfig =  Config (buildDirRelative ".deb-build") Nothing

type Build = ReaderT BaseDir (ReaderT Config IO)

runIO :: IO a -> Build a
runIO =  lift . lift

runBuild :: Build a -> BaseDir -> Config -> IO a
runBuild b =  runReaderT . runReaderT b

withCurrentDir :: FilePath -> Build a -> Build a
withCurrentDir dir act = do
  saveDir <- runIO pwd
  runIO $ chdir dir
  r <- act
  runIO $ chdir saveDir
  return r

getBaseDir :: Build FilePath
getBaseDir =  ask >>= maybe (runIO pwd) return . unBaseDir

askConfig :: Build Config
askConfig =  lift ask

withBaseCurrentDir :: Build a -> Build a
withBaseCurrentDir act = do
  baseDir <- getBaseDir
  withCurrentDir baseDir act

getBuildDir :: Build FilePath
getBuildDir = do
  bd <- buildDir <$> askConfig
  (`unBuildDir` bd) <$> getBaseDir

withBuildDir :: (FilePath -> Build a) -> Build a
withBuildDir f = getBuildDir >>= f

removeBuildDir :: Build ()
removeBuildDir = do
  bldDir <- getBuildDir
  runIO $ do
    found <- doesDirectoryExist bldDir
    when found $ rawSystem' ["rm", "-r", bldDir]

debianDirName :: Build FilePath
debianDirName =  do
  mayD <- mayDebianDirName <$> askConfig
  return $ maybe "debian" id mayD

origArchive :: Package -> Build FilePath
origArchive pkg =
  withBuildDir $ \w -> return $ w </> origArchiveName pkg

nativeArchive :: Package -> Build FilePath
nativeArchive pkg =
  withBuildDir $ \w -> return $ w </> nativeArchiveName pkg

sourceDir :: Package -> Build FilePath
sourceDir pkg =
  withBuildDir $ \w -> return $ w </> sourceDirName pkg

setupDebianDir :: FilePath -> Build ()
setupDebianDir srcDir = do
  debDN       <- debianDirName
  baseDir     <- getBaseDir
  runIO $ rawSystem' ["cp", "-a", baseDir </> debDN, srcDir </> "."]

rsyncGenOrigSourceDir :: Package -> Build FilePath
rsyncGenOrigSourceDir pkg = do
  srcDir   <- sourceDir pkg
  debDN    <- debianDirName
  baseDir  <- getBaseDir
  bldDir   <- getBuildDir
  let excludes = [takeFileName d | d <- [bldDir], baseDir `isPrefixOf` d ] ++ [debDN]
  runIO $ do
    createDirectoryIfMissing True $ srcDir
    rawSystem' $ ["rsync", "-auv"] ++ [ "--exclude=" ++ e | e <- excludes] ++ [baseDir </> ".", srcDir </> "." ]
  return srcDir

rsyncGenOrigSources :: Package -> Build (FilePath, FilePath)
rsyncGenOrigSources pkg = do
  srcDir <- rsyncGenOrigSourceDir pkg
  origPath  <- origArchive pkg
  withBuildDir $ runIO . packInDir' (takeFileName srcDir) origPath
  setupDebianDir srcDir
  runIO $ confirmPath srcDir
  return (origPath, srcDir)

rsyncGenNativeSources :: Package -> Build FilePath
rsyncGenNativeSources pkg = do
  srcDir <- rsyncGenOrigSourceDir pkg
  setupDebianDir srcDir
  runIO $ confirmPath srcDir
  return srcDir

rsyncGenSources :: Package -> Build FilePath
rsyncGenSources pkg
  | isNative pkg = rsyncGenNativeSources pkg
  | otherwise    = snd <$> rsyncGenOrigSources   pkg


cabalGenArchive :: Hackage -> Build FilePath
cabalGenArchive hkg = do
  withBaseCurrentDir . runIO $ Cabal.sdist []
  baseDir <- getBaseDir
  let apath = baseDir </> "dist" </> hackageArchive hkg
  runIO $ confirmPath apath
  return apath

cabalGenOrigArchive :: HaskellPackage -> Build FilePath
cabalGenOrigArchive hpkg = do
  origPath <- origArchive $ package hpkg
  apath    <- cabalGenArchive $ hackage hpkg
  runIO $ do
    createDirectoryIfMissing True $ takeDirectory origPath
    renameFile apath origPath
  return origPath

cabalGenOrigSources :: HaskellPackage -> Build (FilePath, FilePath)
cabalGenOrigSources hpkg = do
  origPath <- cabalGenOrigArchive hpkg
  srcDir   <- sourceDir $ package hpkg
  runIO $ do
    unpack origPath
    renameDirectory
      (takeDirectory origPath </> hackageLongName (hackage hpkg))
      srcDir
  setupDebianDir srcDir
  runIO $ confirmPath srcDir
  return (origPath, srcDir)
