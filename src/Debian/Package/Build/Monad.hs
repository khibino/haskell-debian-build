-- |
-- Module      : Debian.Package.Build.Monad
-- Copyright   : 2014 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- This module provides monad types to control build scripts.
module Debian.Package.Build.Monad
       ( Trace, runTrace, traceCommand, traceOut, putLog
       , bracketTrace, bracketTrace_

       , BaseDir, baseDirCurrent, baseDirSpecify

       , askBaseDir, askBuildDir

       , BuildDir, buildDirRelative, buildDirAbsolute

       , Config, defaultConfig, buildDir, mayDebianDirName, sourceExcludes

       , Build, liftTrace, runBuild, askConfig
       , bracketBuild, bracketBuild_
       ) where

import System.FilePath ((</>))
import System.IO (hPutStrLn, hFlush, stderr, stdout)
import Data.Maybe (fromMaybe)
import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Control.Exception (bracket)


readerBracket :: Monad m
             => (m a -> (a -> m b) -> (a -> m c) -> m c)
             -> ReaderT r m a
             -> (a -> ReaderT r m b)
             -> (a -> ReaderT r m c)
             -> ReaderT r m c
readerBracket brkt open close body = do
  r <- ask
  lift $ brkt
    (runReaderT open r)
    (\a -> runReaderT (close a) r)
    (\a -> runReaderT (body a) r)

toBracket_ :: Monad m
           => (m a -> (a -> m b) -> (a -> m c) -> m c)
           -> m a
           -> m b
           -> m c
           -> m c
toBracket_ brkt start end body =
  brkt start (const end) (const body)

-- | Action type with trace flag
type Trace = ReaderT Bool IO

-- | Run 'Trace' monad
runTrace :: Trace a -> Bool -> IO a
runTrace =  runReaderT

traceIO :: IO () -> Trace ()
traceIO printIO = do
  t <- ask
  when t $ lift printIO

-- | bracket for 'Trace' monad
bracketTrace :: Trace a -> (a -> Trace b) -> (a -> Trace c) -> Trace c
bracketTrace =  readerBracket bracket

-- | bracket_ for 'Trace' monad
bracketTrace_ :: Trace a -> Trace b -> Trace c -> Trace c
bracketTrace_ =  toBracket_ bracketTrace

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

-- | Put log stinrg with flush.
putLog :: String -> Trace ()
putLog s = traceIO $ do
  let fh = stdout
  hPutStrLn fh s
  hFlush fh

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
  , sourceExcludes   :: [FilePath]      -- ^ Exclude directories to setup source directory
  } deriving Show

-- | Default configuration
defaultConfig :: (Config, Bool)
defaultConfig =  (Config (buildDirRelative ".debian-build") Nothing [".git", ".hg"], True)

-- | Monad type with build base directory and build configuration.
type Build = ReaderT BaseDir (ReaderT Config Trace)

-- | Lift from 'Trace' monad into monad with 'Build' configuration.
liftTrace :: Trace a -> Build a
liftTrace =  lift . lift

-- | Run 'Build' configuration monad
runBuild :: Build a -> BaseDir -> Config -> Bool -> IO a
runBuild b bd = runTrace . (runReaderT $ runReaderT b bd)

-- | bracket for 'Build' monad
bracketBuild :: Build a -> (a -> Build b) -> (a -> Build c) -> Build c
bracketBuild =  readerBracket $ readerBracket bracketTrace

-- | bracket_ for 'Build' monad
bracketBuild_ :: Build a -> Build b -> Build c -> Build c
bracketBuild_ =  toBracket_ bracketBuild

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
