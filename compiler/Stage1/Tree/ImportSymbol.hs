{-# LANGUAGE_HAZY UnorderedRecords #-}

-- |
-- Parser syntax tree for module import symbols
module Stage1.Tree.ImportSymbol where

import Stage1.Parser
  ( Parser,
    asum,
  )
import Stage1.Position (Position)
import Stage1.Tree.ImportFields (Fields)
import qualified Stage1.Tree.ImportFields as ImportFields
import qualified Stage1.Tree.Marked as Marked

data Symbol
  = -- |
    -- > import M ( x )
    -- >            ^
    Definition
      { variable :: !(Marked.Variable Position)
      }
  | -- |
    -- > import M ( C )
    -- >            ^
    Data
      { typeVariable :: !(Marked.ConstructorIdentifier Position),
        fields :: !Fields
      }
  deriving (Show)

parse :: Parser Symbol
parse =
  asum
    [ definition <$> Marked.parseLiteral,
      datax <$> Marked.parse <*> ImportFields.parse
    ]
  where
    definition variable =
      Definition
        { variable
        }
    datax typeVariable fields =
      Data
        { typeVariable,
          fields
        }
