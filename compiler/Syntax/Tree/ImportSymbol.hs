{-# LANGUAGE_HAZY UnorderedRecords #-}

-- |
-- Parser syntax tree for module import symbols
module Syntax.Tree.ImportSymbol where

import Syntax.Parser
  ( Parser,
    asum,
  )
import Syntax.Position (Position)
import Syntax.Tree.ImportFields (Fields)
import qualified Syntax.Tree.ImportFields as ImportFields
import qualified Syntax.Tree.Marked as Marked

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
