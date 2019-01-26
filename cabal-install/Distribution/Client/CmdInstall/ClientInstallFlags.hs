{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module Distribution.Client.CmdInstall.ClientInstallFlags
( BindirMethod(..)
, ClientInstallFlags(..)
, defaultClientInstallFlags
, clientInstallOptions
) where

import Distribution.Client.Compat.Prelude
import Distribution.Compat.Binary
         ( Binary(..) )

import Distribution.ReadE
         ( ReadE(..), succeedReadE )
import Distribution.Simple.Command
         ( ShowOrParseArgs(..), OptionField(..), option, reqArg )
import Distribution.Simple.Setup
         ( Flag(..), trueArg, flagToList, toFlag )

import Distribution.Client.InstallSymlink
         ( OverwritePolicy(..) )


data BindirMethod = BindirMethodCopy
                  | BindirMethodSymlink
  deriving (Eq, Show, Generic)

instance Binary BindirMethod

data ClientInstallFlags = ClientInstallFlags
  { cinstInstallLibs :: Flag Bool
  , cinstEnvironmentPath :: Flag FilePath
  , cinstOverwritePolicy :: Flag OverwritePolicy
  , cinstBindirMethod :: Flag BindirMethod
  , cinstCopydir :: Flag FilePath
  } deriving (Eq, Show, Generic)

instance Monoid ClientInstallFlags where
  mempty = gmempty
  mappend = (<>)

instance Semigroup ClientInstallFlags where
  (<>) = gmappend

instance Binary ClientInstallFlags

defaultClientInstallFlags :: ClientInstallFlags
defaultClientInstallFlags = ClientInstallFlags
  { cinstInstallLibs = toFlag False
  , cinstEnvironmentPath = mempty
  , cinstOverwritePolicy = toFlag NeverOverwrite
  , cinstBindirMethod = toFlag BindirMethodSymlink
  , cinstCopydir = mempty
  }

clientInstallOptions :: ShowOrParseArgs -> [OptionField ClientInstallFlags]
clientInstallOptions _ =
  [ option [] ["lib"]
    "Install libraries rather than executables from the target package."
    cinstInstallLibs (\v flags -> flags { cinstInstallLibs = v })
    trueArg
  , option [] ["package-env", "env"]
    "Set the environment file that may be modified."
    cinstEnvironmentPath (\pf flags -> flags { cinstEnvironmentPath = pf })
    (reqArg "ENV" (succeedReadE Flag) flagToList)
  , option [] ["overwrite-policy"]
    "How to handle already existing symlinks."
    cinstOverwritePolicy (\v flags -> flags { cinstOverwritePolicy = v })
    $ reqArg
        "always|never"
        readOverwritePolicyFlag
        showOverwritePolicyFlag
  , option [] ["bindir-method"]
    "TODO description."
    cinstBindirMethod (\v flags -> flags { cinstBindirMethod = v })
    $ reqArg
        "copy|symlink"
        readBindirMethodFlag
        showBindirMethodFlag
  , option [] ["copydir"]
    "TODO description"
    cinstCopydir (\v flags -> flags { cinstCopydir = v })
    $ reqArg "DIR" (succeedReadE Flag) flagToList
  ]
  where --TODO promote to top level
    readOverwritePolicyFlag = ReadE $ \case
      "always" -> Right $ Flag AlwaysOverwrite
      "never"  -> Right $ Flag NeverOverwrite
      policy   -> Left  $ "'" <> policy <> "' isn't a valid overwrite policy"
    showOverwritePolicyFlag (Flag AlwaysOverwrite) = ["always"]
    showOverwritePolicyFlag (Flag NeverOverwrite)  = ["never"]
    showOverwritePolicyFlag NoFlag                 = []
    readBindirMethodFlag = ReadE $ \case
      "copy"    -> Right $ Flag BindirMethodCopy
      "symlink" -> Right $ Flag BindirMethodSymlink
      method    -> Left  $ "'" <> method <> "' isn't a valid bindir-method"
    showBindirMethodFlag (Flag BindirMethodCopy)    = ["copy"]
    showBindirMethodFlag (Flag BindirMethodSymlink) = ["symlink"]
    showBindirMethodFlag NoFlag                     = []
