module Stage2.Group.Tree.TypeDeclaration where

import qualified Data.Vector.Strict as Strict
import qualified Graph.StronglyConnected as StronglyConnected
import Stage1.Position (Position)
import Stage1.Variable (Constructor, ConstructorIdentifier)
import Stage2.Group.Tree.TypeGroup (TypeGroup)
import qualified Stage2.Group.Tree.TypeGroup as TypeGroup
import qualified Stage2.Index.Link.Type as Type
import Stage2.Tree.Type (Type)
import qualified Stage2.Tree.TypeDeclaration as Proper
import Stage2.Tree.TypeDefinition (TypeDefinition)

data TypeDeclaration locality scope
  = Annotated
      { position :: !Position,
        name :: !ConstructorIdentifier,
        constructorNames :: !(Strict.Vector Constructor),
        annotation :: !(Type Position scope),
        definition :: !(TypeDefinition scope)
      }
  | Inferred
      { position :: !Position,
        name :: !ConstructorIdentifier,
        constructorNames :: !(Strict.Vector Constructor),
        definition :: !(TypeDefinition scope),
        meta :: !(TypeGroup locality scope)
      }
  deriving (Show)

group ::
  (Type.Link locality -> Proper.TypeDeclaration scope) ->
  StronglyConnected.Component (Type.Link locality) ->
  Proper.TypeDeclaration scope ->
  TypeDeclaration locality scope
group _ _ Proper.Annotated {position, name, constructorNames, annotation, definition} =
  Annotated {position, name, constructorNames, annotation, definition}
group index component Proper.Inferred {position, name, constructorNames, definition} =
  Inferred
    { position,
      name,
      constructorNames,
      definition,
      meta = TypeGroup.group index component
    }

proper :: TypeDeclaration locality scope -> Proper.TypeDeclaration scope
proper = \case
  Annotated {position, name, constructorNames, annotation, definition} ->
    Proper.Annotated {position, name, constructorNames, annotation, definition}
  Inferred {position, name, constructorNames, definition} ->
    Proper.Inferred {position, name, constructorNames, definition}
