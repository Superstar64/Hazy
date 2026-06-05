{-# LANGUAGE_HAZY UnorderedRecords #-}

-- |
-- Parser syntax tree for expression update fields
module Syntax.Tree.ExpressionField where

import qualified Data.Strict.Vector1 as Strict (Vector1, fromNonEmpty)
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Syntax.Parser
  ( Parser,
    asum,
    betweenBraces,
    sepBy1Comma,
    sepByComma,
    token,
    (<**>),
  )
import Syntax.Position (Position)
import {-# SOURCE #-} Syntax.Tree.Expression (Expression)
import {-# SOURCE #-} qualified Syntax.Tree.Expression as Expression
import qualified Syntax.Tree.Marked as Marked

data Field position
  = -- |
    -- > C { x = e }
    -- >     ^^^^^
    Field
      { variable :: !(Marked.QualifiedVariable position),
        field :: !(Expression position)
      }
  | -- |
    -- > C { x }
    -- >     ^
    Pun
      {variable :: !(Marked.QualifiedVariable position)}
  deriving (Show)

parse :: Parser (Field Position)
parse = Marked.parseLiteral <**> asum options
  where
    field field variable = Field {variable, field}
    pun variable = Pun {variable}
    options =
      [ field <$ token "=" <*> Expression.parse,
        pure pun
      ]

parseMany :: Parser (Strict.Vector (Field Position))
parseMany = Strict.Vector.fromList <$> betweenBraces (sepByComma parse)

parseSome :: Parser (Strict.Vector1 (Field Position))
parseSome = Strict.fromNonEmpty <$> betweenBraces (sepBy1Comma parse)
