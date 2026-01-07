module Stage1.Tree.TypePattern where

import Stage1.Parser (Parser)
import qualified Stage1.Parser as Parser
import Stage1.Position (Position)
import Stage1.Variable (VariableIdentifier)
import qualified Stage1.Variable as Variable

data TypePattern position = TypePattern
  { position :: !position,
    name :: !VariableIdentifier
  }
  deriving (Show)

parse :: Parser (TypePattern Position)
parse = typePattern <$> Parser.position <*> Variable.parse
  where
    typePattern position name = TypePattern {position, name}
