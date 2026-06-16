-- |
-- The language extension table
module Syntax.Extensions (Extensions (..), haskell2010, hazy) where

data Extensions = Extensions
  { implicitPrelude :: !Bool,
    stableImports :: !Bool,
    unorderedRecords :: !Bool,
    constructorFields :: !Bool,
    permissiveUpdates :: !Bool,
    hygienicHiding :: !Bool
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
      hygienicHiding = False
    }

hazy :: Extensions
hazy =
  Extensions
    { implicitPrelude = True,
      stableImports = False,
      unorderedRecords = False,
      constructorFields = True,
      permissiveUpdates = True,
      hygienicHiding = True
    }
