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

  copyDebianDir, setupDebianDir,

  rsyncGenOrigSources,
  rsyncGenNativeSources,
  rsyncGenSources,

  cabalGenOrigSources,
  cabalGenSources,
  cabalAutogenSources
  ) where

import System.FilePath ((</>), takeFileName, takeDirectory)
import System.Directory
  (createDirectoryIfMissing, getDirectoryContents,
   doesDirectoryExist, doesFileExist)
import Control.Applicative ((<$>))
import Control.Monad (when, filterM)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.List (isPrefixOf, isSuffixOf)

import Debian.Package.Internal (rawSystem')
import Debian.Package.Hackage (Hackage, hackageLongName, hackageArchive)
import Debian.Package.Source
  (Package, origArchiveName, nativeArchiveName, sourceDirName, isNative,
   HaskellPackage, hackage, package, parsePackageFromChangeLog, haskellPackageFromPackage)
import Debian.Package.Command
  (pwd, chdir, confirmPath, renameFile, renameDirectory, unpack, packInDir', cabalDebian)
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
  , callCabalDebian  :: Bool              -- ^ Call cabal-debian when hackage case
  } deriving Show

defaultConfig :: Config
defaultConfig =  Config (buildDirRelative ".deb-build") Nothing True

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
  withCurrentDir srcDir . runIO $ cabalDebian mayRev

setupDebianDir :: FilePath -> Build ()
setupDebianDir srcDir = callCabalDebian <$> askConfig >>= setup  where
  setup call
    | call       =  cabalDebianDir (Just "1") srcDir
    | otherwise  =  copyDebianDir srcDir


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
  withBuildDir $ runIO . packInDir' (takeFileName srcDir) origPath
  copyDebianDir srcDir
  runIO $ confirmPath srcDir
  return (origPath, srcDir)

rsyncGenNativeSources :: Package -> Build (FilePath, FilePath)
rsyncGenNativeSources pkg = do
  srcDir <- rsyncGenOrigSourceDir pkg
  copyDebianDir srcDir
  nativePath <- nativeArchive pkg
  withBuildDir $ runIO . packInDir' (takeFileName srcDir) nativePath
  runIO $ confirmPath srcDir
  return (nativePath, srcDir)

rsyncGenSources :: Package -> Build (FilePath, FilePath)
rsyncGenSources pkg
  | isNative pkg = rsyncGenNativeSources pkg
  | otherwise    = rsyncGenOrigSources   pkg


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
  runIO $ confirmPath srcDir
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
  bldDir   <-  getBuildDir

  runIO $ createDirectoryIfMissing True bldDir
  cabalDebianDir Nothing baseDir
  runIO $ renameFile tmpDD bldDir
  return $ bldDir </> ddName

cabalAutogenSources :: String -> Build (FilePath, FilePath)
cabalAutogenSources hname = do
  debDir   <-  cabalAutogenDebianDir
  pkg      <-  runIO . parsePackageFromChangeLog $ debDir </> "changelog"
  hpkg     <-  either fail return $ haskellPackageFromPackage hname pkg
  pair@(_, srcDir)  <-  cabalGenOrigSources hpkg
  bldDir   <-  getBuildDir
  runIO $ renameDirectory (bldDir </> "debian") srcDir
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

findDotCabal :: MaybeT Build FilePath
findDotCabal =  MaybeT $ do
  baseDir  <-  getBaseDir
  runIO $ do
    fs  <-  getDirectoryContents baseDir
    let find f
          | length f > length suf  &&
            suf `isSuffixOf` f           =  doesFileExist $ baseDir </> f
          | otherwise                    =  return False
          where suf = ".cabal"
    fmap (baseDir </>) . listToMaybe <$> filterM find fs

mayBuild :: MaybeT Build a -> BaseDir -> Config -> IO (Maybe a)
mayBuild =  runBuild . runMaybeT
