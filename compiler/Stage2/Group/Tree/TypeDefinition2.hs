module Stage2.Group.Tree.TypeDefinition2 where

import Stage1.Lexer (ConstructorIdentifier)
import Stage1.Position (Position)
import Stage2.Layout (Normal)
import qualified Stage2.Tree.TypeDeclaration as Proper (TypeDeclaration (..))
import qualified Stage2.Tree.TypeDefinition2 as Proper (Inferred, TypeDefinition2 (..))

data TypeDefinition2 locality scope = Inferred
  { position :: !Position,
    name :: !ConstructorIdentifier,
    definition :: !(Proper.TypeDefinition2 locality Proper.Inferred Normal scope)
  }
  deriving (Show)

group :: Proper.TypeDeclaration locality Normal scope -> TypeDefinition2 locality scope
group Proper.Inferred {position, name, definition'} = Inferred {position, name, definition = definition'}
group _ = error "bad group type declaration"
