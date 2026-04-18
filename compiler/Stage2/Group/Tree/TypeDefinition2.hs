module Stage2.Group.Tree.TypeDefinition2 where

import Stage1.Lexer (ConstructorIdentifier)
import Stage1.Position (Position)
import Stage2.Tree.TypeDefinition (TypeDefinition)

data TypeDefinition2 scope = Inferred
  { position :: !Position,
    name :: !ConstructorIdentifier,
    definition :: !(TypeDefinition scope)
  }
  deriving (Show)
