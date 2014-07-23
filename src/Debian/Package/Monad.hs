
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


type Trace = ReaderT Bool IO

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

traceCommand :: String -> Trace ()
traceCommand =  traceIO . tprint '+'

traceOut :: String -> Trace ()
traceOut =  traceIO . tprint '>'


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
  , trace            :: Bool
  } deriving Show

defaultConfig :: Config
defaultConfig =  Config (buildDirRelative ".deb-build") Nothing True

type Build = ReaderT BaseDir (ReaderT Config IO)

liftTrace :: Trace a -> Build a
liftTrace t = lift . ReaderT $ runTrace t . trace

runBuild :: Build a -> BaseDir -> Config -> IO a
runBuild b =  runReaderT . runReaderT b

askBaseDir :: FilePath -> Build FilePath
askBaseDir cur = fromMaybe cur . unBaseDir <$> ask

askConfig :: Build Config
askConfig =  lift ask

askBuildDir :: FilePath -> Build FilePath
askBuildDir cur = do
  bd <- buildDir <$> askConfig
  (`unBuildDir` bd) <$> askBaseDir cur
