{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Distribution.Types.Dependency
  ( Dependency(..)
  , depPkgName
  , depVerRange
  , thisPackageVersion
  , notThisPackageVersion
  , simplifyDependency
  , parsingDependencyToDependencies
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Version ( VersionRange, thisVersion
                            , notThisVersion, anyVersion
                            , simplifyVersionRange )

import qualified Distribution.Compat.ReadP as Parse
import Distribution.Compat.CharParsing (char)
import Distribution.Compat.Parsing (between, option)

import Distribution.Text
import Distribution.Pretty
import Distribution.Parsec.Class
import Distribution.Types.ComponentName
import Distribution.Types.PackageId
import Distribution.Types.PackageName
import Distribution.Types.UnqualComponentName
  (UnqualComponentName, mkUnqualComponentName, unUnqualComponentName)

import Text.PrettyPrint ((<+>))
import Data.Set (Set)
import qualified Data.Set as Set

-- | Describes a dependency on a source package (API)
--
data Dependency = Dependency PackageName VersionRange --ComponentName
                  deriving (Generic, Read, Show, Eq, Typeable, Data)

depPkgName :: Dependency -> PackageName
depPkgName (Dependency pn _) = pn

depVerRange :: Dependency -> VersionRange
depVerRange (Dependency _ vr) = vr

instance Binary Dependency
instance NFData Dependency where rnf = genericRnf

instance Pretty Dependency where
    pretty (Dependency name ver) = pretty name <+> pretty ver


instance Text Dependency where
  parse = do name <- parse
             Parse.skipSpaces
             ver <- parse Parse.<++ return anyVersion
             Parse.skipSpaces
             return (Dependency name ver)

thisPackageVersion :: PackageIdentifier -> Dependency
thisPackageVersion (PackageIdentifier n v) =
  Dependency n (thisVersion v)

notThisPackageVersion :: PackageIdentifier -> Dependency
notThisPackageVersion (PackageIdentifier n v) =
  Dependency n (notThisVersion v)

-- | Simplify the 'VersionRange' expression in a 'Dependency'.
-- See 'simplifyVersionRange'.
--
simplifyDependency :: Dependency -> Dependency
simplifyDependency (Dependency name range) =
  Dependency name (simplifyVersionRange range)

data ParsingDependency = ParsingDependency
                             PackageName
                             VersionRange
                             (Set UnqualComponentName)
                         deriving (Generic, Read, Show, Eq, Typeable, Data)

instance Parsec ParsingDependency where
    parsec = do
        name <- lexemeParsec
        ver  <- parsec <|> pure anyVersion
        comps <- option []
               $ between (char '{') (char '}')
               $ parsecCommaList parsecUnqualComponentName
        return (ParsingDependency name ver (Set.fromList $ mkUnqualComponentName <$> comps))

parsingDependencyToDependencies :: ParsingDependency -> [Dependency]
parsingDependencyToDependencies
    (ParsingDependency pname vr sublibs)
        | Set.null sublibs = [Dependency pname vr {-CLibName-}]
        | otherwise        = []--Dependency pname vr . toComponent <$> Set.toList sublibs
    where
        toComponent cname | unUnqualComponentName cname == unPackageName pname = CLibName
                          | otherwise = CSubLibName cname

