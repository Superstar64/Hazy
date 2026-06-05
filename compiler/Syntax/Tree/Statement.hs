{-# LANGUAGE_HAZY UnorderedRecords #-}

-- |
-- Parser syntax tree for statements and guards
module Syntax.Tree.Statement where

import Syntax.Parser (Parser, asum, position, token, try)
import Syntax.Position (Position)
import Syntax.Tree.Declarations (Declarations)
import qualified Syntax.Tree.Declarations as Declarations
import {-# SOURCE #-} Syntax.Tree.Expression (Expression)
import {-# SOURCE #-} qualified Syntax.Tree.Expression as Expression
import Syntax.Tree.Pattern (Pattern)
import qualified Syntax.Tree.Pattern as Pattern

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
