import System.Environment (getProgName, getArgs)
import Control.Monad (void)
import Data.List (stripPrefix)

import Debian.Package.Data (Source, Hackage)
import Debian.Package.Build
  (BuildMode (Bin, Src), buildPackage, debi,
   baseDirCurrent, defaultConfig, Build, runBuild, liftTrace,
   sourceDir, removeBuildDir, genSources, removeGhcLibrary)


defualtModes :: [BuildMode]
defualtModes =  [Bin, Src]

remove' :: Hackage -> Build ()
remove' hkg = liftTrace $ sequence_ [removeGhcLibrary m hkg | m <- defualtModes]

install' :: Source -> Build ()
install' src = do
  srcDir <- sourceDir src
  liftTrace $ debi srcDir []


help :: IO ()
help =  do
  prog <- getProgName
  let rev  = "[--revision=<debian revision>]"
      opts = "[debuild options]"
  putStr . unlines $ map unwords
    [[prog, "clean"],
     [prog, "source", rev],
     [prog, "build", rev, opts],
     [prog, "install", rev, opts],
     [prog, "reinstall", rev, opts],
     ["-- reinstall (Remove and install) support only for Haskell"],
     ["-- Revision string may use on auto-generating debian directory."]]

clean :: Build ()
clean =  removeBuildDir

source :: Maybe String -> Build ((FilePath, FilePath), Source, Maybe Hackage)
source mayRev = do
  clean
  maybe (fail "Illegal state: genSources") return =<< genSources mayRev

build :: Maybe String -> [String] -> Build (Source, Maybe Hackage)
build mayRev opts = do
  ((_, dir), src, mayH) <- source mayRev
  liftTrace $ sequence_ [buildPackage dir m opts | m <- defualtModes]
  return (src, mayH)

install :: Maybe String -> [String] -> Build ()
install mayRev args = do
  (src, _mayH) <- build mayRev args
  install' src

reinstall :: Maybe String -> [String] -> Build ()
reinstall mayRev args = do
  (src, mayH) <- build mayRev args
  maybe (return ()) remove' mayH
  install' src

run :: Build a -> IO a
run b = uncurry (runBuild b baseDirCurrent) defaultConfig

parseArgs :: [String] -> (Maybe String, [String])
parseArgs =  d where
  d aas@(a:as) = case stripPrefix "--revision=" a of
    r@(Just _)  ->  (r, as)
    Nothing     ->  (Nothing, aas)
  d []         =    (Nothing, [])

main :: IO ()
main =  do
  as0 <- getArgs
  case as0 of
    "-h"     : _          ->  help
    "--help" : _          ->  help
    "help"   : _          ->  help
    as2@(c : as1)  ->  do
      let (mayRev, args) = parseArgs as1
      run $ case c of
        "clean"         ->    clean
        "source"        ->    void $ source mayRev
        "build"         ->    void $ build mayRev args
        "install"       ->    install mayRev args
        "reinstall"     ->    reinstall mayRev args
        _               ->    void . uncurry build $ parseArgs as2

    []                  -> run . void $ build Nothing []
