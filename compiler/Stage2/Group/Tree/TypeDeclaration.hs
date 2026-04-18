module Stage2.Group.Tree.TypeDeclaration where

import Stage1.Position (Position)
import Stage1.Variable (ConstructorIdentifier)
import Stage2.Group.Tree.TypeGroup (TypeGroup)
import Stage2.Tree.Type (Type)
import Stage2.Tree.TypeDefinition (TypeDefinition)

data TypeDeclaration scope
  = Annotated
      { position :: !Position,
        name :: !ConstructorIdentifier,
        annotation :: !(Type Position scope),
        definition :: !(TypeDefinition scope)
      }
  | Group !(TypeGroup scope)
  deriving (Show)
