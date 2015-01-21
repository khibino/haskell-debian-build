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

       , ChangesType (..), takeChangesType

       , HaskellPackage, hackage, package
       , haskellPackageDefault, haskellPackageFromPackage
       ) where

import Control.Arrow (second)
import Control.Applicative ((<$>), pure, (<*>), (<|>))
import Control.Monad.Trans.State (StateT, runStateT)
import Control.Monad.Trans.Class (lift)
import Numeric (readDec)
import Data.Maybe (listToMaybe)
import Data.Char (isSpace)
import Data.Version (Version (Version, versionBranch), showVersion, parseVersion)
import Data.List.Split (splitOn)
import Text.ParserCombinators.ReadP (ReadP, string, readP_to_S, readS_to_P)
import System.FilePath ((<.>), takeFileName, splitExtension)

import Debian.Package.Data.Hackage
  (HackageVersion, mkHackageVersion', hackageVersionNumbers,
   Hackage, mkHackageDefault, NameRule (Simple), debianNamesFromSourceName)


-- For base-4.5.0.0 - debian wheezy, portability trick.
-- Monad m ==> Applicative (StateT s m)
type Parser = StateT () ReadP

runParser :: Parser a -> String -> [(a, String)]
runParser p in0 = [ (x, in1) | ((x, ()), in1) <- readP_to_S (runStateT p ()) in0 ]


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

lexWord :: String -> [(String, String)]
lexWord =  (:[]) . break isSpace . dropWhile isSpace

returnParsed :: Parser a -> String -> Parser a
returnParsed p s = case [ x | (x, "") <- runParser p s ] of
  [x] -> return x
  []  -> fail "ReadP: no parse"
  _   -> fail "ReadP: ambiguous parse"

returnParsedVersion :: String -> Parser Version
returnParsedVersion =  returnParsed $ lift parseVersion

returnParsedNMU :: String -> Parser (Maybe Int)
returnParsedNMU = returnParsed $
                  Just <$> lift (string "+nmu" >> readS_to_P readDec) <|>
                  pure Nothing

parseDebianVersion :: Parser DebianVersion
parseDebianVersion =  do
  vs0 <- lift $ readS_to_P lexWord
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
  readsPrec _ = runParser parseDebianVersion

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
