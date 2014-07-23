
module Debian.Package.Monad
       ( Trace, runTrace, traceCommand, traceOut

       , BaseDir, baseDirCurrent, baseDirSpecify

       , askBaseDir, askBuildDir

       , BuildDir, buildDirRelative, buildDirAbsolute

       , Config, defaultConfig, buildDir, mayDebianDirName, trace

       , Build, liftTrace, runBuild, askConfig
       ) where

import System.FilePath ((</>))
import System.IO (hPutStrLn, hFlush, stderr)
import Data.Maybe (fromMaybe)
import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT (ReaderT), ask, runReaderT)


-- | Monad type with trace flag
type Trace = ReaderT Bool IO

-- | Run 'Trace' monad
runTrace :: Trace a -> Bool -> IO a
runTrace =  runReaderT

traceIO :: IO () -> Trace ()
traceIO printIO = do
  t <- ask
  when t $ lift printIO

tprint :: Char -> String -> IO ()
tprint pc s = do
  let fh = stderr
  hPutStrLn fh $ pc : " " ++ s
  hFlush fh

-- | Command string trace print along with trace flag
traceCommand :: String -> Trace ()
traceCommand =  traceIO . tprint '+'

-- | Output string trace print along with trace flag
traceOut :: String -> Trace ()
traceOut =  traceIO . tprint '>'


-- | Type to specify base directory filepath
newtype BaseDir = BaseDir { unBaseDir :: Maybe FilePath }

-- | Use current directory as base directory
baseDirCurrent :: BaseDir
baseDirCurrent =  BaseDir Nothing

-- | Use specified directory as base directory
baseDirSpecify :: FilePath -> BaseDir
baseDirSpecify =  BaseDir . Just

-- | Type to specify build working directory
newtype BuildDir = BuildDir (Either FilePath FilePath)

-- | Use relative path from base-dir as build workding directory
buildDirRelative :: FilePath -> BuildDir
buildDirRelative = BuildDir . Left

-- | Use absolute path as build workding directory
buildDirAbsolute :: FilePath -> BuildDir
buildDirAbsolute =  BuildDir . Right

-- | Fold build dir
unBuildDir :: FilePath -> BuildDir -> FilePath
unBuildDir base (BuildDir b) = either (base </>) id b

-- | Show 'BuildDir' is relative or absolute
instance Show BuildDir where
  show = d  where
    d (BuildDir (Left  p)) = "Relative " ++ p
    d (BuildDir (Right p)) = "Absolute " ++ p

-- | Build configuration type
data Config =
  Config
  { buildDir         :: BuildDir        -- ^ Specify build dir
  , mayDebianDirName :: Maybe FilePath  -- ^ Name of debian directory
  , trace            :: Bool            -- ^ Trace flag used when 'liftTrace'
  } deriving Show

-- | Default configuration
defaultConfig :: Config
defaultConfig =  Config (buildDirRelative ".deb-build") Nothing True

-- | Monad type with build base directory and build configuration.
type Build = ReaderT BaseDir (ReaderT Config IO)

-- | Lift from 'Trace' monad into monad with 'Build' configuration.
liftTrace :: Trace a -> Build a
liftTrace t = lift . ReaderT $ runTrace t . trace

-- | Run 'Build' configuration monad
runBuild :: Build a -> BaseDir -> Config -> IO a
runBuild b =  runReaderT . runReaderT b

-- | Get base directory in 'Build' monad
askBaseDir :: FilePath -> Build FilePath
askBaseDir cur = fromMaybe cur . unBaseDir <$> ask

-- | Get build configuration in 'Build' monad
askConfig :: Build Config
askConfig =  lift ask

-- | Get build working directory in 'Build' monad
askBuildDir :: FilePath -> Build FilePath
askBuildDir cur = do
  bd <- buildDir <$> askConfig
  (`unBuildDir` bd) <$> askBaseDir cur
