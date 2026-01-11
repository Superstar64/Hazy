{-# LANGUAGE_HAZY UnorderedRecords #-}

-- |
-- Parser syntax tree for pattern fields
module Stage1.Tree.PatternField where

import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Stage1.Parser
  ( Parser,
    asum,
    betweenBraces,
    sepByComma,
    token,
    (<**>),
  )
import Stage1.Position (Position)
import Stage1.TermBindingVariables (TermBindingVariables (..))
import Stage1.Tree.Marked (Marked (..))
import qualified Stage1.Tree.Marked as Marked
import {-# SOURCE #-} Stage1.Tree.Pattern (Pattern)
import {-# SOURCE #-} qualified Stage1.Tree.Pattern as Pattern
import Stage1.Variable
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
