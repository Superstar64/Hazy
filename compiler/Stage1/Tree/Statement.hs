-- |
-- Parser syntax tree for statements and guards
module Stage1.Tree.Statement where

import Stage1.Parser (Parser, asum, token, try)
import Stage1.Position (Position)
import Stage1.Tree.Declarations (Declarations)
import qualified Stage1.Tree.Declarations as Declarations
import {-# SOURCE #-} Stage1.Tree.Expression (Expression)
import {-# SOURCE #-} qualified Stage1.Tree.Expression as Expression
import Stage1.Tree.Pattern (Pattern)
import qualified Stage1.Tree.Pattern as Pattern

data Statement position
  = Run !(Expression position)
  | -- |
    -- > x <- e
    Bind !(Pattern position) !(Expression position)
  | -- |
    -- > let x = e
    Let2 !(Declarations position)
  deriving (Show)

parse :: Parser (Statement Position)
parse =
  asum
    [ Bind <$> try (Pattern.parse <* token "<-") <*> Expression.parse,
      Run <$> try Expression.parse,
      Let2 <$> (token "let" *> Declarations.parse)
    ]
