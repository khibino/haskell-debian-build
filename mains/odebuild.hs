import Control.Arrow ((>>>))
import Control.Monad (void, when)
import System.Environment (getProgName, getArgs)
import System.Console.GetOpt
  (OptDescr (Option), ArgDescr (ReqArg, NoArg), ArgOrder (RequireOrder),
   usageInfo, getOpt)

import Debian.Package.Data (Source, Hackage, isBinaryPackage)
import qualified Debian.Package.Build.Command as Command
import Debian.Package.Build
  (BuildMode (Dep, Indep, Src), debi', pwd,
   defaultConfig, Build, runBuild, liftTrace,
   removeBuildDir, findDebianChanges, genSources, removeGhcLibrary)


remove' :: Hackage -> Build ()
remove' hkg = liftTrace $ sequence_ [removeGhcLibrary m hkg | m <- [Dep, Indep, Src]]

install' :: Build ()
install' =  do
  ps <- findDebianChanges
  let cs = [c | (c, t) <- ps, isBinaryPackage t ]
  liftTrace $ do
    when (null cs) $ fail "No .changes files found!"
    mapM_ (\c -> debi' [c]) cs


data ODebuildOptions =
  ODebuildOptions
  { revision    :: Maybe String
  , installDeps :: Bool
  } deriving Show

defaultOptions :: ODebuildOptions
defaultOptions =
  ODebuildOptions
  { revision     =  Nothing
  , installDeps  =  False
  }

descs :: [OptDescr (ODebuildOptions -> ODebuildOptions)]
descs =
  [ Option [] ["revision"]
    (ReqArg (\s opts -> opts { revision = Just s }) "DEBIAN_REVISION")
    "debian package revision to pass to cabal-debian"
  , Option [] ["install-deps"]
    (NoArg $ \opts -> opts { installDeps = True })
    "install build depends when to run build"
  ]

parseOption :: [String]
            -> (ODebuildOptions -> ODebuildOptions, ([String], [String]))
parseOption args = (foldr (>>>) id ufs, (ss1, ss2))  where
  (ufs, ss1, ss2) = getOpt RequireOrder descs args

help :: IO ()
help =  do
  prog <- getProgName
  let opts  = "[options]"
      debOpts = "[-- [debuild options]]"
      msg = unlines $ map unwords
            [ [prog, "clean"]
            , [prog, "source", opts]
            , [prog, "build", opts, debOpts]
            , [prog, "install", opts, debOpts]
            , [prog, "reinstall", opts, debOpts]
            , ["  reinstall (Remove and install) support only for Haskell"]
            , ["  Revision string may use on auto-generating debian directory."]
            ]
  putStr $ usageInfo msg descs

clean :: Build ()
clean =  removeBuildDir

source :: ODebuildOptions -> Build ((FilePath, FilePath), Source, Maybe Hackage)
source opts = do
  clean
  maybe (fail "Illegal state: genSources") return =<< genSources (revision opts)

build' :: [BuildMode] -> ODebuildOptions -> [String] -> Build (Source, Maybe Hackage)
build' modes opts args = do
  ((_, dir), src, mayH) <- source opts
  liftTrace $ Command.build dir modes (installDeps opts) args
  return (src, mayH)

build :: ODebuildOptions -> [String] -> Build (Source, Maybe Hackage)
build =  build' []

install :: ODebuildOptions -> [String] -> Build ()
install opts args = do
  void $ build opts args
  install'

reinstall :: ODebuildOptions -> [String] -> Build ()
reinstall opts args = do
  (_src, mayH) <- build opts args
  maybe (return ()) remove' mayH
  install'

run :: Build a -> IO a
run b = do
  cur <- pwd
  uncurry (runBuild b cur) defaultConfig

parseArgs :: [String] -> IO (ODebuildOptions, [String])
parseArgs args0
  | not $ null errs  = fail $ concat errs
  | not $ null args1 = fail $ "Unknown arguments: " ++ unwords args1
  | otherwise        = return
                       (f defaultOptions, drop 1 rest )  where
  (opt, rest) = break (== "--") args0
  (f, (args1, errs)) = parseOption opt

runArgs :: (ODebuildOptions -> [String] -> Build a) -> [String] -> IO a
runArgs act as = do
  (opts, args) <- parseArgs as
  run $ act opts args

main :: IO ()
main =  do
  as0 <- getArgs
  case as0 of
    "-h"     : _          ->  help
    "--help" : _          ->  help
    "help"   : _          ->  help
    as2@(c : as1)  ->  do
      case c of
        "clean"         ->    run clean
        "source"        ->    void $ runArgs (const . source) as1
        "build"         ->    void $ runArgs build as1
        "install"       ->    runArgs install as1
        "reinstall"     ->    runArgs reinstall as1
        _               ->    void $ runArgs build as2

    []                  -> run . void $ build defaultOptions []
