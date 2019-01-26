{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module Distribution.Client.CmdInstall.ClientInstallFlags
( BindirMethod(..)
, ClientInstallFlags(..)
, defaultClientInstallFlags
, clientInstallOptions
) where

import Distribution.Client.Compat.Prelude

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
  deriving (Eq, Show, Generic, Bounded, Enum)

instance Binary BindirMethod

data ClientInstallFlags = ClientInstallFlags
  { cinstInstallLibs :: Flag Bool
  , cinstEnvironmentPath :: Flag FilePath
  , cinstOverwritePolicy :: Flag OverwritePolicy
  , cinstBindirMethod :: Flag BindirMethod --TODO bindir-method or install-method?
  , cinstInstalldir :: Flag FilePath
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
  , cinstInstalldir = mempty
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
    "How to install the executables."
    cinstBindirMethod (\v flags -> flags { cinstBindirMethod = v })
    $ reqArg
        "copy|symlink"
        readBindirMethodFlag
        showBindirMethodFlag
  , option [] ["installdir"]
    "Where to install (by symlinking or copying) the executables in."
    cinstInstalldir (\v flags -> flags { cinstInstalldir = v })
    $ reqArg "DIR" (succeedReadE Flag) flagToList
  ]

readOverwritePolicyFlag :: ReadE (Flag OverwritePolicy)
readOverwritePolicyFlag = ReadE $ \case
  "always" -> Right $ Flag AlwaysOverwrite
  "never"  -> Right $ Flag NeverOverwrite
  policy   -> Left  $ "'" <> policy <> "' isn't a valid overwrite policy"

showOverwritePolicyFlag :: Flag OverwritePolicy -> [String]
showOverwritePolicyFlag (Flag AlwaysOverwrite) = ["always"]
showOverwritePolicyFlag (Flag NeverOverwrite)  = ["never"]
showOverwritePolicyFlag NoFlag                 = []

readBindirMethodFlag :: ReadE (Flag BindirMethod)
readBindirMethodFlag = ReadE $ \case
  "copy"    -> Right $ Flag BindirMethodCopy
  "symlink" -> Right $ Flag BindirMethodSymlink
  method    -> Left  $ "'" <> method <> "' isn't a valid bindir-method"

showBindirMethodFlag :: Flag BindirMethod -> [String]
showBindirMethodFlag (Flag BindirMethodCopy)    = ["copy"]
showBindirMethodFlag (Flag BindirMethodSymlink) = ["symlink"]
showBindirMethodFlag NoFlag                     = []

