import System.Environment (getProgName, getArgs)
import Control.Monad (void)

import Debian.Package.Data (Source, Hackage)
import Debian.Package.Build
  (BuildMode (All), buildPackage, debi,
   baseDirCurrent, defaultConfig, Build, runBuild, liftTrace,
   sourceDir, removeBuildDir, genSources, removeGhcLibrary)


remove' :: Hackage -> Build ()
remove' =  liftTrace . removeGhcLibrary All

install' :: Source -> Build ()
install' src = do
  srcDir <- sourceDir src
  liftTrace $ debi srcDir []


help :: IO ()
help =  do
  prog <- getProgName
  let opts = "<debuild options>"
  putStr . unlines $ map unwords
    [[prog, "clean"],
     [prog, "source"],
     [prog, "build", opts],
     [prog, "install", opts],
     [prog, "reinstall", opts, "  -- Remove and install support only for Haskell"] ]

clean :: Build ()
clean =  removeBuildDir

source :: Build ((FilePath, FilePath), Source, Maybe Hackage)
source =  do
  clean
  maybe (fail "Illegal state: genSources") return =<< genSources

build :: [String] -> Build (Source, Maybe Hackage)
build opts = do
  ((_, dir), src, mayH) <- source
  liftTrace $ buildPackage dir All opts
  return (src, mayH)

install :: [String] -> Build ()
install args = do
  (src, _mayH) <- build args
  install' src

reinstall :: [String] -> Build ()
reinstall args = do
  (src, mayH) <- build args
  maybe (return ()) remove' mayH
  install' src

run :: Build a -> IO a
run b = uncurry (runBuild b baseDirCurrent) defaultConfig

main :: IO ()
main =  do
  as0 <- getArgs
  case as0 of
    "-h"     : _          ->  help
    "--help" : _          ->  help
    "help"   : _          ->  help
    as1  ->  run $ case  as1  of
      "clean" : _         ->  clean
      "source" : _        ->  void $ source
      "build" : args      ->  void $ build args
      "install" : args    ->  install args
      "reinstall" : args  ->  reinstall args
      args                ->  void $ build args
