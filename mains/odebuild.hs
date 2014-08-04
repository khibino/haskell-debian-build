import System.Environment (getProgName, getArgs)
import Control.Monad (void)

import Debian.Package.Data (Source, Hackage)
import Debian.Package.Build
  (BuildMode (All), buildPackage, debi,
   baseDirCurrent, defaultConfig, Build, runBuild, liftTrace,
   sourceDir, removeBuildDir, genSources, removeGhcLibrary)


source' :: Build ((FilePath, FilePath), Source, Maybe Hackage)
source' =
  maybe (fail "Illegal state: genSources") return =<< genSources

remove' :: Hackage -> Build ()
remove' =  liftTrace . removeGhcLibrary All

install' :: Source -> Build ()
install' src = do
  srcDir <- sourceDir src
  liftTrace $ debi srcDir []


help :: IO ()
help =  do
  prog <- getProgName
  putStr . unlines $ map unwords
    [[prog, "build"],
     [prog, "install"],
     [prog, "reinstall", "  -- Remove and install support only for Haskell"] ]

build :: [String] -> Build (Source, Maybe Hackage)
build opts = do
  removeBuildDir
  ((_, dir), src, mayH) <- source'
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
    "help" : _            ->  help
    as1  ->  run $ case  as1  of
      "build" : args      ->  void $ build args
      "install" : args    ->  install args
      "reinstall" : args  ->  reinstall args
      args                ->  void $ build args
