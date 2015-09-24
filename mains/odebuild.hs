import Control.Monad (void, when, (>=>))
import Data.Maybe (listToMaybe)
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
  , buildModes  :: [BuildMode] -> [BuildMode]
  }

defaultOptions :: ODebuildOptions
defaultOptions =
  ODebuildOptions
  { revision     =  Nothing
  , installDeps  =  False
  , buildModes   =  id
  }

descs :: [OptDescr (ODebuildOptions -> Either String ODebuildOptions)]
descs =
  [ Option [] ["revision"]
    (ReqArg (\s opts -> return $ opts { revision = Just s }) "DEBIAN_REVISION")
    "debian package revision to pass to cabal-debian"
  , Option [] ["install-deps"]
    (NoArg $ \opts   -> return $ opts { installDeps = True })
    "install build depends before running build"
  , Option [] ["mode"]
    (ReqArg (\s opts -> maybe (Left $ "Unknown build mode: " ++ s) Right $ do
                m <- listToMaybe [m | (m, "") <- reads s]
                return $ opts { buildModes = buildModes opts . (m : ) } )
     "BUILD_MODE")
    "add build-mode to build-mode list to specify"
  ]

parseOption :: [String]
            -> (ODebuildOptions -> Either String ODebuildOptions, ([String], [String]))
parseOption args = (foldr (>=>) return ufs, (as, es))  where
  (ufs, as, es) = getOpt RequireOrder descs args

help :: IO ()
help =  do
  prog <- getProgName
  let opts  = "[options]"
      debOpts = "[-- [debuild options]]"
      msg = unlines $ map unwords
            [ [prog, "clean"]
            , [prog, "prepare", opts]
            , [prog, "source", opts, debOpts]
            , [prog, "build", opts, debOpts]
            , [prog, "install", opts, debOpts]
            , [prog, "reinstall", opts, debOpts]
            , ["  reinstall (Remove and install) support only for Haskell"]
            , ["  Revision string may use on auto-generating debian directory."]
            ]
  putStr $ usageInfo msg descs

clean :: Build ()
clean =  removeBuildDir

prepare :: ODebuildOptions -> Build ((FilePath, FilePath), Source, Maybe Hackage)
prepare opts = do
  clean
  maybe (fail "Illegal state: genSources") return =<< genSources (revision opts)

build' :: [BuildMode] -> ODebuildOptions -> [String] -> Build (Source, Maybe Hackage)
build' modes opts args = do
  ((_, dir), src, mayH) <- prepare opts
  liftTrace $ Command.build dir modes (installDeps opts) args
  return (src, mayH)

source :: ODebuildOptions -> [String] -> Build (Source, Maybe Hackage)
source = build' [Src]

build :: ODebuildOptions -> [String] -> Build (Source, Maybe Hackage)
build opts = build' (buildModes opts []) opts

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
  | not $ null errs  = fail $ '\n' : concat [ "  " ++ e | e <- errs ]
  | not $ null args1 = fail $ "Unknown arguments: " ++ unwords args1
  | otherwise        =
      either (fail . ("Option parse error: " ++)) return $ do
        opts <- f defaultOptions
        return (opts, drop 1 rest)
      where
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
        "prepare"       ->    void $ runArgs (const . prepare) as1
        "source"        ->    void $ runArgs source as1
        "build"         ->    void $ runArgs build as1
        "install"       ->    runArgs install as1
        "reinstall"     ->    runArgs reinstall as1
        _               ->    void $ runArgs build as2

    []                  -> run . void $ build defaultOptions []
