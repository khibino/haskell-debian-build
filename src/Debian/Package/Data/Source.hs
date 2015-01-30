-- |
-- Module      : Debian.Package.Data.Source
-- Copyright   : 2014 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- This module provides data types of debian source package meta information.
module Debian.Package.Data.Source
       ( DebianVersion, versionFromHackageVersion, readDebianVersion, origVersion', isNative'

       , Source, mkSource, sourceName, version, origVersion, isNative

       , origArchiveName, nativeArchiveName, sourceDirName, deriveHackageVersion

       , parseChangeLog

       , ChangesType (..), takeChangesType, isSourceChanges, isBinaryChanges

       , HaskellPackage, hackage, package
       , haskellPackageDefault, haskellPackageFromPackage
       ) where

import Control.Arrow (second)
import Control.Applicative ((<$>), pure, (<*>), (*>), empty, (<|>), many, some, optional)
import Control.Monad.Trans.State (StateT, runStateT, get, put)
import Data.Maybe (listToMaybe, maybeToList)
import Data.Char (isSpace, isDigit)
import Data.Version (Version (Version, versionBranch), showVersion)
import Data.List.Split (splitOn)
import System.FilePath ((<.>), takeFileName, splitExtension)

import Debian.Package.Data.Hackage
  (HackageVersion, mkHackageVersion', hackageVersionNumbers,
   Hackage, mkHackageDefault, NameRule (Simple), debianNamesFromSourceName)


type Parser = StateT String Maybe

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
  s <- get
  case s of
    c:cs -> if p c
            then  put cs *> pure c
            else  empty
    []   ->       empty

char :: Char -> Parser Char
char x = satisfy (== x)

_look :: Parser String
_look =  get

_eof :: Parser ()
_eof =  do
  s <- get
  case s of
    []   -> pure ()
    _:_  -> empty

runParser :: Parser a -> String -> Maybe (a, String)
runParser =  runStateT

digit :: Parser Char
digit =  satisfy isDigit

int :: Parser Int
int =  read <$> some digit

string' :: String -> Parser String
string' =  mapM char


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
  d (Just rev) = debianNonNativeVersion ns rev
  d Nothing    = debianNativeVersion    ns Nothing
  ns = hackageVersionNumbers hv

-- | Version without debian revision
origVersion' :: DebianVersion -> Version
origVersion' =  d  where
  d (DebianNative    v _) = v
  d (DebianNonNative v _) = v

-- | Is debian-native or not
isNative' :: DebianVersion -> Bool
isNative' = d where
  d (DebianNative    _ _) = True
  d (DebianNonNative _ _) = False

parseVersion' :: Parser Version
parseVersion' =
  Version
  <$> ((:) <$> int <*> many (char '.' *> int))
  <*> pure []

parseDebianVersion' :: Parser DebianVersion
parseDebianVersion' = do
  v <- parseVersion'
  (DebianNonNative v <$> (char '-' *> some (satisfy (not . isSpace)))
   <|>
   DebianNative    v <$> optional (string' "+nmu" *> int))

_testParseDebianVersion :: [Maybe (DebianVersion, String)]
_testParseDebianVersion =
  [ runParser parseDebianVersion' s | s <- [ "1.23.3-4", "1.23", "12.3+nmu2" ] ]

instance Show DebianVersion where
  show = d  where
    d (DebianNative    v nr) = showVersion v ++ maybe "" (("+nmu" ++) . show) nr
    d (DebianNonNative v r)  = showVersion v ++ '-': r

instance Read DebianVersion where
  readsPrec _ = maybeToList . runParser parseDebianVersion'

readMaybe' :: Read a => String -> Maybe a
readMaybe' =  fmap fst . listToMaybe . filter ((== "") . snd) . reads

-- | Try to read debian package version
readDebianVersion :: String -> Maybe DebianVersion
readDebianVersion =  readMaybe'

-- | Debian source package type, name with version
data Source = Source String DebianVersion  deriving Show

-- | Make 'Source'
mkSource :: String -> DebianVersion -> Source
mkSource =  Source

-- | Source package name of 'Source'
sourceName :: Source -> String
sourceName (Source n _) = n

-- | Debian version of 'Source'
version :: Source -> DebianVersion
version (Source _ v) = v

-- | Version without debian revision
origVersion :: Source -> Version
origVersion =  origVersion' . version

-- | Is debian-native or not
isNative :: Source -> Bool
isNative =  isNative' . version

-- | Original source archive basename
origArchiveName :: Source -> FilePath
origArchiveName pkg = sourceName pkg ++ '_' : showVersion (origVersion pkg) <.> "orig" <.> "tar" <.> "gz"

-- | Debian native archive basename
nativeArchiveName :: Source -> String
nativeArchiveName pkg = sourceName pkg ++ '_' : show (version pkg) <.> "tar" <.> "gz"

-- | Source directory basename
sourceDirName :: Source -> FilePath
sourceDirName pkg = sourceName pkg ++ '-' : showVersion (origVersion pkg)

-- | Try to make 'HackageVersion' from 'Source'
deriveHackageVersion :: Source -> HackageVersion
deriveHackageVersion =  mkHackageVersion' . versionBranch . origVersion where

-- | Try to generate 'Source' from debian changelog string
parseChangeLog :: String       -- ^ dpkg-parsechangelog result string
               -> Maybe Source -- ^ Source structure
parseChangeLog log' = do
  deb  <- mayDebSrc
  dver <- mayDebVer
  return $ mkSource deb dver
  where
    pairs = map (second tail . break (== ' ')) . lines $ log'
    lookup' = (`lookup` pairs)
    mayDebSrc = lookup' "Source:"
    mayDebVer = do
      dverS <- lookup' "Version:"
      readDebianVersion dverS

-- | Debian .changes file types
data ChangesType
  = ChangesArch String
  | ChangesAll
  | ChangesSource
  deriving (Eq, Show)

-- | Take 'ChangesType' from debian .changes file path
takeChangesType :: FilePath -> Maybe ChangesType
takeChangesType path = d . splitExtension $ takeFileName path  where
  d (n, ".changes") = case xs of
    [_, _, a] -> case a of
      "all"    -> Just   ChangesAll
      "source" -> Just   ChangesSource
      _        -> Just $ ChangesArch a
    _          -> Nothing
    where xs = splitOn "_" n
  d (_, _)     =  Nothing

-- | Test changes file type is source package.
isSourceChanges :: ChangesType -> Bool
isSourceChanges = d where
  d (ChangesArch _) = False
  d  ChangesAll     = False
  d  ChangesSource  = True

-- | Test changes file type is binary package.
isBinaryChanges :: ChangesType -> Bool
isBinaryChanges = not . isSourceChanges


-- | Debian source package type for Haskell
data HaskellPackage = HaskellPackage Hackage Source deriving Show

-- | 'Hackage' meta-info of 'HaskellPackage'
hackage :: HaskellPackage -> Hackage
hackage (HaskellPackage h _) = h

-- | Debian source package meta-info of 'HaskellPackage'
package :: HaskellPackage -> Source
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
  (mkSource sn (versionFromHackageVersion hver mayDevRev))
  where
    (sn, _) = debianNamesFromSourceName rule hname

-- | Generate 'HaskellPackage' with hackage name and debian package meta-info
haskellPackageFromPackage :: String         -- ^ Hackage name string
                          -> Source         -- ^ Debian package meta info
                          -> HaskellPackage -- ^ Result
haskellPackageFromPackage hname pkg = HaskellPackage hkg pkg  where
  hv  = deriveHackageVersion pkg
  hkg = mkHackageDefault Simple hname hv
