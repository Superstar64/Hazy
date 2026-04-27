module Stage2.Group.Tree.TypeDeclaration where

import qualified Graph.StronglyConnected as StronglyConnected
import Stage1.Position (Position)
import Stage1.Variable (ConstructorIdentifier)
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
        annotation :: !(Type Position scope),
        definition :: !(TypeDefinition scope)
      }
  | Inferred
      { position :: !Position,
        name :: !ConstructorIdentifier,
        definition :: !(TypeDefinition scope),
        meta :: !(TypeGroup locality scope)
      }
  deriving (Show)

group ::
  (Type.Link locality -> Proper.TypeDeclaration scope) ->
  StronglyConnected.Component (Type.Link locality) ->
  Proper.TypeDeclaration scope ->
  TypeDeclaration locality scope
group _ _ Proper.Annotated {position, name, annotation, definition} =
  Annotated {position, name, annotation, definition}
group index component Proper.Inferred {position, name, definition} =
  Inferred
    { position,
      name,
      definition,
      meta = TypeGroup.group index component
    }

proper :: TypeDeclaration locality scope -> Proper.TypeDeclaration scope
proper = \case
  Annotated {position, name, annotation, definition} ->
    Proper.Annotated {position, name, annotation, definition}
  Inferred {position, name, definition} ->
    Proper.Inferred {position, name, definition}
