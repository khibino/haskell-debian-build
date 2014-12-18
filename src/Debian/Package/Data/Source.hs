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

import Debian.Package.Data.Hackage
  (HackageVersion, mkHackageVersion', hackageVersionNumbers,
   Hackage, mkHackageDefault, NameRule (Simple), debianNamesFromSourceName)


-- Combinators like Applicative -- for base-4.5.0.0 - debian wheezy

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
haskellPackageFromPackage :: String                       -- ^ Hackage name string
                          -> Source                       -- ^ Debian package meta info
                          -> Either String HaskellPackage -- ^ Result
haskellPackageFromPackage hname pkg = do
  let hv  = deriveHackageVersion pkg
      hkg = mkHackageDefault Simple hname hv
  return $ HaskellPackage hkg pkg
