{-# LANGUAGE_HAZY UnorderedRecords #-}

-- |
-- Parser syntax tree for statements and guards
module Stage1.Tree.Statement where

import Stage1.Parser (Parser, asum, position, token, try)
import Stage1.Position (Position)
import Stage1.Tree.Declarations (Declarations)
import qualified Stage1.Tree.Declarations as Declarations
import {-# SOURCE #-} Stage1.Tree.Expression (Expression)
import {-# SOURCE #-} qualified Stage1.Tree.Expression as Expression
import Stage1.Tree.Pattern (Pattern)
import qualified Stage1.Tree.Pattern as Pattern

data Statement position
  = Run
      { startPosition :: !position,
        expression :: !(Expression position)
      }
  | -- |
    -- > x <- e
    Bind
      { startPosition :: !position,
        patternx :: !(Pattern position),
        expression :: !(Expression position)
      }
  | -- |
    -- > let x = e
    Let
      { startPosition :: !position,
        declarations :: !(Declarations position)
      }
  deriving (Show)

parse :: Parser (Statement Position)
parse =
  asum
    [ bind <$> position <*> try (Pattern.parse <* token "<-") <*> Expression.parse,
      run <$> position <*> try Expression.parse,
      letx <$> position <*> (token "let" *> Declarations.parse)
    ]
  where
    bind startPosition patternx expression = Bind {startPosition, patternx, expression}
    run startPosition expression = Run {startPosition, expression}
    letx startPosition declarations = Let {startPosition, declarations}
