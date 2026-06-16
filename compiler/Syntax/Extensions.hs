-- |
-- The language extension table
module Syntax.Extensions (Extensions (..), haskell2010, hazy) where

data Extensions = Extensions
  { implicitPrelude :: !Bool,
    stableImports :: !Bool,
    unorderedRecords :: !Bool,
    constructorFields :: !Bool,
    permissiveUpdates :: !Bool,
    hygenicHiding :: !Bool
  }
  deriving (Show)

haskell2010 :: Extensions
haskell2010 =
  Extensions
    { implicitPrelude = True,
      stableImports = False,
      unorderedRecords = False,
      constructorFields = False,
      permissiveUpdates = False,
      hygenicHiding = False
    }

hazy :: Extensions
hazy =
  Extensions
    { implicitPrelude = True,
      stableImports = False,
      unorderedRecords = False,
      constructorFields = True,
      permissiveUpdates = True,
      hygenicHiding = True
    }
