{-# LANGUAGE_HAZY UnorderedRecords #-}
module Syntax.Tree.TypePattern where

import Syntax.Parser (Parser)
import qualified Syntax.Parser as Parser
import Syntax.Position (Position)
import Syntax.Variable (VariableIdentifier)
import qualified Syntax.Variable as Variable

data TypePattern position = TypePattern
  { position :: !position,
    name :: !VariableIdentifier
  }
  deriving (Show)

parse :: Parser (TypePattern Position)
parse = typePattern <$> Parser.position <*> Variable.parse
  where
    typePattern position name = TypePattern {position, name}
