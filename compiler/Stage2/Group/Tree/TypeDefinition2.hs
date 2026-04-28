module Stage2.Group.Tree.TypeDefinition2 where

import Stage1.Lexer (ConstructorIdentifier)
import Stage1.Position (Position)
import qualified Stage2.Tree.TypeDeclaration as Proper
import Stage2.Tree.TypeDefinition (TypeDefinition)

data TypeDefinition2 scope = Inferred
  { position :: !Position,
    name :: !ConstructorIdentifier,
    definition :: !(TypeDefinition scope)
  }
  deriving (Show)

group :: Proper.TypeDeclaration locality scope -> TypeDefinition2 scope
group Proper.Inferred {position, name, definition} = Inferred {position, name, definition}
group _ = error "bad group type declaration"
