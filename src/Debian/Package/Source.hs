
module Debian.Package.Source
       ( DebianVersion, versionFromHackageVersion

       , Package, mkPackage, sourceName, version, origVersion, isNative

       , origArchiveName, nativeArchiveName, sourceDirName, deriveHackageVersion

       , packageFromChangeLog, parsePackageFromChangeLog

       , HaskellPackage, hackage, package
       , haskellPackageDefault, haskellPackageFromPackage
       ) where

import Data.Maybe (listToMaybe)
import Control.Arrow (second)
import Control.Monad (ap, MonadPlus, mplus)
import Numeric (readDec)
import Data.Char (isSpace)
import Data.Version (Version (Version, versionBranch), showVersion, parseVersion)
import Text.ParserCombinators.ReadP (ReadP, string, readP_to_S, readS_to_P)
import System.FilePath ((<.>))

import Debian.Package.Internal (tarGz)
import Debian.Package.Hackage
  (HackageVersion, mkHackageVersion, hackageVersionNumbers,
   Hackage, mkHackageDefault, NameRule (Simple), debianNamesFromSourceName)
import Debian.Package.Monad (Trace)
import Debian.Package.Command (readProcess')


-- Combinators like Applicative -- for base-4.5.0.0

(<$>) :: Functor m => (a -> b) -> m a -> m b
(<$>) =  fmap

pure :: Monad m => a -> m a
pure =  return

(<*>) :: Monad m => m (a -> b) -> m a -> m b
(<*>) =  ap

(*>) :: Monad m => m a -> m b -> m b
(*>) =  (>>)

--  (<*) :: (Functor m, Monad m) => m a -> m b -> m a
--  fa <* fb = const <$> fa <*> fb

(<|>) :: MonadPlus m => m a -> m a -> m a
(<|>) =  mplus

infixl 3 <|>
infixl 4 <$>, <*>, *>


-- | Version type for Debian
data DebianVersion
  = DebianNative    Version (Maybe Int)
  | DebianNonNative Version String

debianNativeVersion :: [Int] -> Maybe Int -> DebianVersion
debianNativeVersion v =  DebianNative (Version v [])

debianNonNativeVersion :: [Int] -> String -> DebianVersion
debianNonNativeVersion v = DebianNonNative (Version v [])

-- | Make deebian version from hackage version
versionFromHackageVersion :: HackageVersion -> Maybe String -> DebianVersion
versionFromHackageVersion hv = d where
  d (Just rev) = debianNonNativeVersion [v0, v1, v2, v3] rev
  d Nothing    = debianNativeVersion    [v0, v1, v2, v3] Nothing
  (v0, v1, v2, v3) = hackageVersionNumbers hv

origVersion' :: DebianVersion -> Version
origVersion' =  d  where
  d (DebianNative    v _) = v
  d (DebianNonNative v _) = v

isNative' :: DebianVersion -> Bool
isNative' = d where
  d (DebianNative    _ _) = True
  d (DebianNonNative _ _) = False

lexWord :: String -> [(String, String)]
lexWord =  (:[]) . break isSpace . dropWhile isSpace

returnParsed :: ReadP a -> String -> ReadP a
returnParsed p s = case [ x | (x, "") <- readP_to_S p s ] of
  [x] -> return x
  []  -> fail "ReadP: no parse"
  _   -> fail "ReadP: ambiguous parse"

returnParsedVersion :: String -> ReadP Version
returnParsedVersion =  returnParsed parseVersion

returnParsedNMU :: String -> ReadP (Maybe Int)
returnParsedNMU = returnParsed $
                  Just <$> (string "+nmu" *> readS_to_P readDec)  <|>
                  pure Nothing

parseDebianVersion :: ReadP DebianVersion
parseDebianVersion =  do
  vs0 <- readS_to_P lexWord
  let (vs1, rtag) = break (== '-') vs0
      (vs2, nmu)  = break (== '+') vs1
  if rtag == ""
    then DebianNative    <$> returnParsedVersion vs2 <*> returnParsedNMU nmu
    else DebianNonNative <$> returnParsedVersion vs1 <*> return (tail rtag)

instance Show DebianVersion where
  show = d  where
    d (DebianNative    v nr) = showVersion v ++ maybe "" (("+nmu" ++) . show) nr
    d (DebianNonNative v r)  = showVersion v ++ '-': r

instance Read DebianVersion where
  readsPrec _ = readP_to_S parseDebianVersion

readMaybe' :: Read a => String -> Maybe a
readMaybe' =  fmap fst . listToMaybe . filter ((== "") . snd) . reads


-- | Debian source package type, name with version
data Package = Package String DebianVersion  deriving Show

-- | Make 'Package'
mkPackage :: String -> DebianVersion -> Package
mkPackage =  Package

-- | Source package name of 'Package'
sourceName :: Package -> String
sourceName (Package n _) = n

-- | Debian version of 'Package'
version :: Package -> DebianVersion
version (Package _ v) = v

-- | Version without debian revision
origVersion :: Package -> Version
origVersion =  origVersion' . version

-- | Is debian-native or not
isNative :: Package -> Bool
isNative =  isNative' . version

-- | Original source archive basename
origArchiveName :: Package -> FilePath
origArchiveName pkg = sourceName pkg ++ '_' : showVersion (origVersion pkg) <.> "orig" <.> tarGz

-- | Debian native archive basename
nativeArchiveName :: Package -> String
nativeArchiveName pkg = sourceName pkg ++ '_' : show (version pkg) <.> tarGz

-- | Source directory basename
sourceDirName :: Package -> FilePath
sourceDirName pkg = sourceName pkg ++ '-' : showVersion (origVersion pkg)

-- | Try to make 'HackageVersion' from 'Package'
deriveHackageVersion :: Package -> Maybe HackageVersion
deriveHackageVersion =  d . versionBranch . origVersion where
  d [v0, v1, v2, v3] = Just $ mkHackageVersion v0 v1 v2 v3
  d _                = Nothing

-- | Try to generate 'Package' from debian changelog string
packageFromChangeLog :: String        -- ^ dpkg-parsechangelog result string
                     -> Maybe Package -- ^ Package structure
packageFromChangeLog log' = do
  deb  <- mayDebSrc
  dver <- mayDebVer
  return $ Package deb dver
  where
    pairs = map (second tail . break (== ' ')) . lines $ log'
    lookup' = (`lookup` pairs)
    mayDebSrc = lookup' "Source:"
    mayDebVer = do
      dverS <- lookup' "Version:"
      readMaybe' dverS

-- | Read debian changelog file and try to parse into 'Package'
parsePackageFromChangeLog :: FilePath -> Trace Package
parsePackageFromChangeLog cpath =  do
  str <- readProcess' ["dpkg-parsechangelog", "-l" ++ cpath]
  maybe (fail $ "parsePackageFromChangeLog: failed: " ++ str) return
    $ packageFromChangeLog str


-- | Debian source package type for Haskell
data HaskellPackage = HaskellPackage Hackage Package deriving Show

-- | 'Hackage' meta-info of 'HaskellPackage'
hackage :: HaskellPackage -> Hackage
hackage (HaskellPackage h _) = h

-- | Debian source package meta-info of 'HaskellPackage'
package :: HaskellPackage -> Package
package (HaskellPackage _ p) = p

-- | Generate 'HaskellPackage' type from debian package name and version
--   using 'NameRule'
haskellPackageDefault :: NameRule
                      -> String         -- ^ Hackage name string
                      -> HackageVersion -- ^ Version of hackage
                      -> Maybe String   -- ^ Debian revision String
                      -> HaskellPackage -- ^ Result structure
haskellPackageDefault rule hname hver mayDevRev =
  HaskellPackage
  (mkHackageDefault rule hname hver)
  (mkPackage sn (versionFromHackageVersion hver mayDevRev))
  where
    (sn, _) = debianNamesFromSourceName rule hname

-- | Generate 'HaskellPackage' with hackage name and debian package meta-info
haskellPackageFromPackage :: String                       -- ^ Hackage name string
                          -> Package                      -- ^ Debian package meta info
                          -> Either String HaskellPackage -- ^ Result
haskellPackageFromPackage hname pkg = do
  hv <- maybe (Left "Fail to derive hackage version") Right
        $ deriveHackageVersion pkg
  let hkg = mkHackageDefault Simple hname hv
  return $ HaskellPackage hkg pkg
