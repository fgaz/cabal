{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Distribution.Types.LibraryDependency (
  LibraryDependency(..)
) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Types.ComponentName
import Distribution.Types.PackageName
import Distribution.Types.VersionRange

data LibraryDependency =
  LibraryDependency
    PackageName
    -- TODO LibraryName = LLibName | LSubLibName UnqualComponentName
    -- and ComponentName = ... | CLibName LibraryName
    ComponentName
    VersionRange
  deriving (Generic, Read, Show, Eq, Typeable, Data)

instance Binary LibraryDependency
instance NFData LibraryDependency where rnf = genericRnf

