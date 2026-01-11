{-# LANGUAGE_HAZY UnorderedRecords #-}

-- |
-- Parser syntax tree for statment groups
module Stage1.Tree.Statements (Statements (..), parseComprehension, parseDo) where

import Data.Foldable (toList)
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Stage1.Parser (Parser, asum, optional, sepByComma, token, try)
import Stage1.Position (Position)
import qualified Stage1.Tree.Declarations as Declarations
import {-# SOURCE #-} Stage1.Tree.Expression (Expression)
import {-# SOURCE #-} qualified Stage1.Tree.Expression as Expression
import qualified Stage1.Tree.Pattern as Pattern
import Stage1.Tree.Statement (Statement (..))
import qualified Stage1.Tree.Statement as Statement

data Statements position = Statements
  { body :: !(Strict.Vector (Statement position)),
    done :: !(Expression position)
  }
  deriving (Show)

parseComprehension :: Parser (Expression Position -> Statements Position)
parseComprehension = statements . Strict.Vector.fromList <$> sepByComma Statement.parse
  where
    statements body done = Statements {body, done}

data Statements' position = Statements' [Statement position] (Expression position)

parseDo :: Parser (Statements Position)
parseDo = run <$> parseDo'

run (Statements' body done) =
  Statements
    { body = Strict.Vector.fromList body,
      done
    }

parseDo' :: Parser (Statements' Position)
parseDo' =
  asum
    [ statement
        <$> try
          ((bind <$> try (Pattern.parse <* token "<-") <*> Expression.parse) <* token ";")
        <*> parseDo',
      statement' <$> try (try Expression.parse <* token ";") <*> optional parseDo',
      statement <$> try ((Let <$> (token "let" *> Declarations.parse)) <* token ";") <*> parseDo',
      Statements' [] <$> Expression.parse
    ]
  where
    bind patternx expression = Bind {patternx, expression}
    statement statement1 (Statements' statements expression) =
      Statements' (statement1 : toList statements) expression
    statement' expression Nothing = Statements' [] expression
    statement' expression (Just (Statements' statements expression')) =
      Statements' (Run expression : statements) expression'
