{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.PackageVersionConstraint
( PackageVersionConstraint(..)
, thisPackageVersion
, notThisPackageVersion
, simplifyPackageVersionConstraint
) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Types.PackageId
import Distribution.Types.PackageName
import Distribution.Types.VersionRange

import Distribution.Version ( VersionRange, thisVersion
                            , notThisVersion, anyVersion
                            , simplifyVersionRange )

import Distribution.Pretty
import Distribution.Text
import Distribution.Parsec.Class
import qualified Distribution.Compat.ReadP as Parse
import Text.PrettyPrint ((<+>))

data PackageVersionConstraint =
  PackageVersionConstraint { pkgVerConstraintPkgName :: PackageName
                           , pkgVerConstraintVerRange :: VersionRange }
  deriving (Generic, Read, Show, Eq, Typeable, Data)

instance Binary PackageVersionConstraint
instance NFData PackageVersionConstraint where rnf = genericRnf

instance Parsec PackageVersionConstraint where
    parsec = do
        name <- lexemeParsec
        ver  <- parsec <|> pure anyVersion
        return (PackageVersionConstraint name ver)

instance Pretty PackageVersionConstraint where
    pretty (PackageVersionConstraint name ver) = pretty name <+> pretty ver

instance Text PackageVersionConstraint where
  parse = do name <- parse
             Parse.skipSpaces
             ver <- parse Parse.<++ return anyVersion
             Parse.skipSpaces
             return (PackageVersionConstraint name ver)

thisPackageVersion :: PackageIdentifier -> PackageVersionConstraint
thisPackageVersion (PackageIdentifier n v) =
  PackageVersionConstraint n (thisVersion v)

notThisPackageVersion :: PackageIdentifier -> PackageVersionConstraint
notThisPackageVersion (PackageIdentifier n v) =
  PackageVersionConstraint n (notThisVersion v)

-- | Simplify the 'VersionRange' expression in a 'Dependency'.
-- See 'simplifyVersionRange'.
--
simplifyPackageVersionConstraint :: PackageVersionConstraint -> PackageVersionConstraint
simplifyPackageVersionConstraint (PackageVersionConstraint name range) =
  PackageVersionConstraint name (simplifyVersionRange range)

