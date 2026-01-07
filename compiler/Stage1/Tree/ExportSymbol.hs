-- |
-- Parser syntax tree for module export symbols
module Stage1.Tree.ExportSymbol where

import Stage1.Parser
  ( Parser,
    asum,
    token,
  )
import Stage1.Position (Position)
import Stage1.Tree.ImportFields (Fields)
import qualified Stage1.Tree.ImportFields as ImportFields
import qualified Stage1.Tree.Marked as Marked

data Symbol
  = -- |
    -- > module M ( x )
    -- >            ^
    Definition
      { variable :: !(Marked.QualifiedVariable Position)
      }
  | -- |
    -- > module M ( X )
    -- >            ^
    Data
      { typeVariable :: !(Marked.QualifiedConstructorIdentifier Position),
        fields :: !Fields
      }
  | -- |
    -- > module M ( module A )
    -- >            ^^^^^^^^
    Module
      { modulex :: !(Marked.FullQualifiers Position)
      }
  deriving (Show)

parse :: Parser Symbol
parse =
  asum
    [ definition <$> Marked.parseLiteral,
      datax <$> Marked.parse <*> ImportFields.parse,
      token "module" *> (modulex <$> Marked.parse)
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
    modulex modulex =
      Module
        { modulex
        }
