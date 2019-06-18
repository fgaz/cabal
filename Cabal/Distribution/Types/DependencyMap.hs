module Distribution.Types.DependencyMap (
    DependencyMap,
    toDepMap,
    fromDepMap,
    constrainBy,
) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Types.Dependency
import Distribution.Types.PackageName
import Distribution.Types.LibraryName
import Distribution.Version

import Data.Set (Set)
import qualified Data.Map.Lazy as Map

-- | A map of dependencies.  Newtyped since the default monoid instance is not
--   appropriate.  The monoid instance uses 'intersectVersionRanges'.
newtype DependencyMap = DependencyMap { unDependencyMap :: Map PackageName (VersionRange, Set LibraryName, DependencySyntax) }
  deriving (Show, Read)

instance Monoid DependencyMap where
    mempty = DependencyMap Map.empty
    mappend = (<>)

instance Semigroup DependencyMap where
    (DependencyMap a) <> (DependencyMap b) =
        DependencyMap (Map.unionWith intersectVersionRangesAndJoinComponents a b)

intersectVersionRangesAndJoinComponents :: (VersionRange, Set LibraryName, DependencySyntax)
                                        -> (VersionRange, Set LibraryName, DependencySyntax)
                                        -> (VersionRange, Set LibraryName, DependencySyntax)
intersectVersionRangesAndJoinComponents (va, ca, syna) (vb, cb, synb) =
  (intersectVersionRanges va vb, ca <> cb, syna <> synb) -- XXX This is WRONG and has always been

toDepMap :: [Dependency] -> DependencyMap
toDepMap ds =
  DependencyMap $ Map.fromListWith intersectVersionRangesAndJoinComponents [ (p,(vr,cs,syn)) | Dependency p vr cs syn <- ds ]

fromDepMap :: DependencyMap -> [Dependency]
fromDepMap m = [ Dependency p vr cs syn | (p,(vr,cs,syn)) <- Map.toList (unDependencyMap m) ]

-- Apply extra constraints to a dependency map.
-- Combines dependencies where the result will only contain keys from the left
-- (first) map.  If a key also exists in the right map, both constraints will
-- be intersected.
constrainBy :: DependencyMap  -- ^ Input map
            -> DependencyMap  -- ^ Extra constraints
            -> DependencyMap
constrainBy left extra =
    DependencyMap $
      Map.foldrWithKey tightenConstraint (unDependencyMap left)
                                         (unDependencyMap extra)
  where tightenConstraint n c l =
            case Map.lookup n l of
              Nothing -> l
              Just vrcs -> Map.insert n (intersectVersionRangesAndJoinComponents vrcs c) l
