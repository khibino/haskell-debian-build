
module Debian.Package.Build.Monad
       ( BaseDir, baseDirCurrent, baseDirSpecify

       , askBaseDir, askBuildDir

       , BuildDir, buildDirRelative, buildDirAbsolute

       , Config, defaultConfig, buildDir, mayDebianDirName

       , Build, runIO, runBuild, askConfig
       ) where

import System.FilePath ((</>))
import Control.Applicative ((<$>))
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)


newtype BaseDir = BaseDir { unBaseDir :: Maybe FilePath }

baseDirCurrent :: BaseDir
baseDirCurrent =  BaseDir Nothing

baseDirSpecify :: FilePath -> BaseDir
baseDirSpecify =  BaseDir . Just

newtype BuildDir = BuildDir (Either FilePath FilePath)

buildDirRelative :: FilePath -> BuildDir
buildDirRelative = BuildDir . Left

buildDirAbsolute :: FilePath -> BuildDir
buildDirAbsolute =  BuildDir . Right

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

askBaseDir :: FilePath -> Build FilePath
askBaseDir cur = ask >>= return . maybe cur id . unBaseDir

askConfig :: Build Config
askConfig =  lift ask

askBuildDir :: FilePath -> Build FilePath
askBuildDir cur = do
  bd <- buildDir <$> askConfig
  (`unBuildDir` bd) <$> askBaseDir cur
