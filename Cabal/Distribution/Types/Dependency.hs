{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
module Distribution.Types.Dependency
  ( Dependency(..)
  , depPkgName
  , depVerRange
  , depLibraries
  , depSyntax
  , DependencySyntax(..)
  , thisPackageVersion
  , notThisPackageVersion
  , simplifyDependency
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Version ( VersionRange, thisVersion
                            , notThisVersion, anyVersion
                            , simplifyVersionRange )

import Distribution.CabalSpecVersion
import Distribution.Pretty
import qualified Text.PrettyPrint as PP
import Distribution.Parsec
import Distribution.Compat.CharParsing (char, spaces)
import Distribution.Compat.Parsing (between, option)
import Distribution.Types.PackageId
import Distribution.Types.PackageName
import Distribution.Types.LibraryName
import Distribution.Types.UnqualComponentName

import Text.PrettyPrint ((<+>))
import Data.Set (Set)
import qualified Data.Set as Set

-- | Describes a dependency on a source package (API)
--
data Dependency = Dependency
                    PackageName
                    VersionRange
                    (Set LibraryName)
                    -- ^ The set of libraries required from the package.
                    -- Only the selected libraries will be built.
                    -- It does not affect the cabal-install solver yet.
                    DependencySyntax
                    -- ^ Which syntax was used
                  deriving (Generic, Read, Show, Eq, Typeable, Data)

data DependencySyntax = DependencySyntaxUnqualified
                      -- ^ Simply @package@ (can also be interpreted as @lib@,
                      -- where @lib@ is an internal sublibrary)
                      | DependencySyntaxQualified
                      -- ^ Fully qualified syntax: @package:sublibrary@
                      -- or @package:{sublibrary1,2,...}@. There's no
                      -- ambiguity here.
                    deriving (Generic, Read, Show, Eq, Typeable, Data)

-- XXX This should not even exist. Combining them is like combining package names and library names
instance Semigroup DependencySyntax where
    DependencySyntaxUnqualified <> DependencySyntaxUnqualified = DependencySyntaxUnqualified
    _                           <> _                           = DependencySyntaxQualified

depPkgName :: Dependency -> PackageName
depPkgName (Dependency pn _ _ _) = pn

depVerRange :: Dependency -> VersionRange
depVerRange (Dependency _ vr _ _) = vr

depLibraries :: Dependency -> Set LibraryName
depLibraries (Dependency _ _ cs _) = cs

depSyntax :: Dependency -> DependencySyntax
depSyntax (Dependency _ _ _ s) = s

instance Binary Dependency
instance NFData Dependency where rnf = genericRnf

instance Binary DependencySyntax
instance NFData DependencySyntax where rnf = genericRnf

instance Pretty Dependency where
    pretty (Dependency name ver sublibs syntax) = pretty name
                                       <+> optionalMonoid
                                             (sublibs /= Set.singleton LMainLibName
                                             -- XXX not sure if I should do this
                                             -- Is it better to roundtrip or to always show the qualified syntax?
                                             || syntax == DependencySyntaxQualified)
                                             (PP.colon <+> PP.braces prettySublibs)
                                       <+> pretty ver
      where
        optionalMonoid True x = x
        optionalMonoid False _ = mempty
        prettySublibs = PP.hsep $ PP.punctuate PP.comma $ prettySublib <$> Set.toList sublibs
        prettySublib LMainLibName = PP.text $ unPackageName name
        prettySublib (LSubLibName un) = PP.text $ unUnqualComponentName un

versionGuardMultilibs :: (Monad m, CabalParsing m) => m a -> m a
versionGuardMultilibs expr = do
  csv <- askCabalSpecVersion
  if csv < CabalSpecV3_0
  then fail $ unwords
    [ "Sublibrary dependency syntax used."
    , "To use this syntax the package needs to specify at least 'cabal-version: 3.0'."
    , "Alternatively, if you are depending on an internal library, you can write"
    , "directly the library name as it were a package."
    ]
  else
    expr

instance Parsec Dependency where
    parsec = do
        name <- lexemeParsec

        (libs, syntax) <- option ([LMainLibName], DependencySyntaxUnqualified) $
          fmap (, DependencySyntaxQualified) $
          (char ':' *> spaces *>) $
          versionGuardMultilibs $
          pure <$> parseLib name <|> parseMultipleLibs name
        ver  <- parsec <|> pure anyVersion
        return $ Dependency name ver (Set.fromList libs) syntax
      where makeLib pn ln | unPackageName pn == ln = LMainLibName
                          | otherwise = LSubLibName $ mkUnqualComponentName ln
            parseLib pn = makeLib pn <$> parsecUnqualComponentName
            parseMultipleLibs pn = between (char '{' *> spaces)
                                           (spaces <* char '}')
                                           $ parsecCommaList $ parseLib pn

-- mempty should never be in a Dependency-as-dependency.
-- This is only here until the Dependency-as-constraint problem is solved #5570.
-- Same for below.
thisPackageVersion :: PackageIdentifier -> Dependency
thisPackageVersion (PackageIdentifier n v) =
  Dependency n (thisVersion v) Set.empty DependencySyntaxQualified

notThisPackageVersion :: PackageIdentifier -> Dependency
notThisPackageVersion (PackageIdentifier n v) =
  Dependency n (notThisVersion v) Set.empty DependencySyntaxQualified

-- | Simplify the 'VersionRange' expression in a 'Dependency'.
-- See 'simplifyVersionRange'.
--
simplifyDependency :: Dependency -> Dependency
simplifyDependency (Dependency name range comps syntax) =
  Dependency name (simplifyVersionRange range) comps syntax
