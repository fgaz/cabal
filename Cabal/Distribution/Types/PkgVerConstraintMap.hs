{-# LANGUAGE CPP #-}

#ifdef MIN_VERSION_containers
#if MIN_VERSION_containers(0,5,0)
#define MIN_VERSION_containers_0_5_0
#endif
#endif

#ifndef MIN_VERSION_containers
#if __GLASGOW_HASKELL__ >= 706
#define MIN_VERSION_containers_0_5_0
#endif
#endif

module Distribution.Types.PkgVerConstraintMap (
    PkgVerConstraintMap,
    lookupPkgVerConstraintMap,
    toPkgVerConstraintMap,
    fromPkgVerConstraintMap,
    constrainBy,
) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Types.PackageVersionConstraint
import Distribution.Types.PackageName
import Distribution.Version

#ifdef MIN_VERSION_containers_0_5_0
import qualified Data.Map.Lazy as Map
#else
import qualified Data.Map as Map
#endif

-- | A map of version constraints.  Newtyped since the default monoid instance
--   is not appropriate.  The monoid instance uses 'intersectVersionRanges'.
newtype PkgVerConstraintMap = PkgVerConstraintMap { unPkgVerConstraintMap :: Map PackageName VersionRange }
  deriving (Show, Read)

instance Monoid PkgVerConstraintMap where
    mempty = PkgVerConstraintMap Map.empty
    mappend = (<>)

instance Semigroup PkgVerConstraintMap where
    (PkgVerConstraintMap a) <> (PkgVerConstraintMap b) =
        PkgVerConstraintMap (Map.unionWith intersectVersionRanges a b)

lookupPkgVerConstraintMap :: PackageName -> PkgVerConstraintMap -> Maybe VersionRange
lookupPkgVerConstraintMap pn (PkgVerConstraintMap m) = Map.lookup pn m

toPkgVerConstraintMap :: [PackageVersionConstraint] -> PkgVerConstraintMap
toPkgVerConstraintMap ds =
  PkgVerConstraintMap $ Map.fromListWith intersectVersionRanges [ (p,vr) | PackageVersionConstraint p vr <- ds ]

fromPkgVerConstraintMap :: PkgVerConstraintMap -> [PackageVersionConstraint]
fromPkgVerConstraintMap m = [ PackageVersionConstraint p vr | (p,vr) <- Map.toList (unPkgVerConstraintMap m) ]

-- Apply extra constraints to a dependency map.
-- Combines dependencies where the result will only contain keys from the left
-- (first) map.  If a key also exists in the right map, both constraints will
-- be intersected.
constrainBy :: PkgVerConstraintMap  -- ^ Input map
            -> PkgVerConstraintMap  -- ^ Extra constraints
            -> PkgVerConstraintMap
constrainBy left extra =
    PkgVerConstraintMap $
#ifdef MIN_VERSION_containers_0_5_0
      Map.foldrWithKey tightenConstraint (unPkgVerConstraintMap left)
                                         (unPkgVerConstraintMap extra)
#else
      Map.foldWithKey tightenConstraint (unPkgVerConstraintMap left)
                                        (unPkgVerConstraintMap extra)
#endif
  where tightenConstraint n c l =
            case Map.lookup n l of
              Nothing -> l
              Just vr -> Map.insert n (intersectVersionRanges vr c) l
