-- |
-- The language extension table
module Stage1.Extensions (Extensions (..), haskell2010, hazy) where

data Extensions = Extensions
  { implicitPrelude :: !Bool,
    stableImports :: !Bool,
    unorderedRecords :: !Bool,
    constructorFields :: !Bool
  }
  deriving (Show)

haskell2010 :: Extensions
haskell2010 =
  Extensions
    { implicitPrelude = True,
      stableImports = False,
      unorderedRecords = False,
      constructorFields = False
    }

hazy :: Extensions
hazy =
  Extensions
    { implicitPrelude = True,
      stableImports = True,
      unorderedRecords = False,
      constructorFields = False
    }
