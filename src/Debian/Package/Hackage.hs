
module Debian.Package.Hackage
       ( HackageVersion, mkHackageVersion, hackageVersionNumbers

       , Hackage, mkHackage, hackageName, hackageVersion
       , debianShortName, mkHackageDefault

       , NameRule (..), debianNamesFromSourceName

       , hackageLongName

       , hackageArchiveName, hackageArchive

       , ghcLibraryBinPackages, ghcLibraryDocPackage, ghcLibraryPackages
       ) where

import Data.List (stripPrefix)
import Data.Char (toLower)
import Data.Version (Version (Version), showVersion, parseVersion)
import Text.ParserCombinators.ReadP (readP_to_S)
import System.FilePath ((</>), (<.>))

import Debian.Package.Internal (tarGz)


newtype HackageVersion = HackageVersion (Version)

mkHackageVersion :: Int -> Int -> Int -> Int -> HackageVersion
mkHackageVersion v0 v1 v2 v3 = HackageVersion $ Version [v0, v1, v2, v3] []

hackageVersionNumbers :: HackageVersion -> (Int, Int, Int, Int)
hackageVersionNumbers = d  where
  d (HackageVersion (Version [v0, v1, v2, v3] [])) = (v0, v1, v2, v3)
  d hv                                             = error $ "HackageVersion: Invalid structure: " ++ show hv

instance Show HackageVersion where
  show (HackageVersion v) = showVersion v

instance Read HackageVersion where
  readsPrec _ = map toH . filter h . readP_to_S parseVersion  where
    h (Version b t, _) = length b == 4 && null t
    toH (v, s) = (HackageVersion v, s)

data Hackage = Hackage String HackageVersion String  deriving Show

mkHackage :: String -> HackageVersion -> String -> Hackage
mkHackage =  Hackage

hackageName :: Hackage -> String
hackageName (Hackage n _ _) = n

hackageVersion :: Hackage -> HackageVersion
hackageVersion (Hackage _ v _) = v

debianShortName :: Hackage -> String
debianShortName (Hackage _ _ sn) = sn

mkHackageDefault :: NameRule       -- ^ Rule flag to generate names
                 -> String         -- ^ Hackage name string
                 -> HackageVersion -- ^ Version of hackage
                 -> Hackage        -- ^ Result hackage meta info
mkHackageDefault rule hname hver = mkHackage hname hver short  where
  (_ , short) = debianNamesFromSourceName rule hname

defaultHackageSrcPrefix :: String
defaultHackageSrcPrefix =  "haskell-"

data NameRule = Suggest | Simple deriving (Eq, Show)

debianNamesFromSourceName :: NameRule          -- ^ Rule flag to generate name
                          -> String            -- ^ Debian source name or Hackage name string
                          -> (String, String)  -- ^ Debian source package name and short name like ("haskell-src-exts", "src-exts")
debianNamesFromSourceName rule hname = d rule  where
  lh = map toLower hname
  d Suggest  =  rec' ["haskell-", "haskell"]
  d Simple   =  (defaultHackageSrcPrefix ++ lh, lh)
  rec' []     = (defaultHackageSrcPrefix ++ lh, lh)
  rec' (p:ps) = case stripPrefix p lh of
    Just s  -> (lh, s)
    Nothing -> rec' ps

hackageLongName :: Hackage -> String
hackageLongName hkg = hackageName hkg ++ '-' : show (hackageVersion hkg)

hackageArchiveName :: Hackage -> FilePath
hackageArchiveName hkg = hackageLongName hkg <.> tarGz

distDir :: String
distDir =  "dist"

hackageArchive :: Hackage -> FilePath
hackageArchive =  (distDir </>) . hackageArchiveName


ghcLibraryBinPackages :: Hackage -> [String]
ghcLibraryBinPackages hkg =
  [ concat ["libghc-", debianShortName hkg, '-' : suf]
  | suf <- ["dev", "prof"]
  ]

ghcLibraryDocPackage :: Hackage -> String
ghcLibraryDocPackage hkg = concat ["libghc-", debianShortName hkg, "-doc"]

ghcLibraryPackages :: Hackage -> [String]
ghcLibraryPackages hkg = ghcLibraryDocPackage hkg : ghcLibraryBinPackages hkg
