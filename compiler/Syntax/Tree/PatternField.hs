{-# LANGUAGE_HAZY UnorderedRecords #-}

-- |
-- Parser syntax tree for pattern fields
module Syntax.Tree.PatternField where

import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Syntax.FreeVariables (TermBindingVariables (..))
import Syntax.Parser
  ( Parser,
    asum,
    betweenBraces,
    sepByComma,
    token,
    (<**>),
  )
import Syntax.Position (Position)
import Syntax.Tree.Marked (Marked (..))
import qualified Syntax.Tree.Marked as Marked
import {-# SOURCE #-} Syntax.Tree.Pattern (Pattern)
import {-# SOURCE #-} qualified Syntax.Tree.Pattern as Pattern
import Syntax.Variable
  ( QualifiedVariable (..),
  )

data Field position
  = -- |
    -- > C { x = x }
    -- >     ^^^^^
    Field
      { variable :: !(Marked.QualifiedVariable position),
        patternx :: !(Pattern position)
      }
  | -- |
    -- > C { x }
    -- >     ^
    Pun
      { variable :: !(Marked.QualifiedVariable position)
      }
  deriving (Show)

instance TermBindingVariables Field where
  termBindingVariables = \case
    Field {patternx} -> termBindingVariables patternx
    Pun {variable = position :@ _ :- variable} -> [position :@ variable]

parseMany :: Parser (Strict.Vector (Field Position))
parseMany = betweenBraces (Strict.Vector.fromList <$> sepByComma parse)

parse :: Parser (Field Position)
parse =
  Marked.parseLiteral
    <**> asum
      [ field <$> (token "=" *> Pattern.parse),
        pure pun
      ]
  where
    field patternx variable =
      Field
        { variable,
          patternx
        }
    pun variable =
      Pun
        { variable
        }
