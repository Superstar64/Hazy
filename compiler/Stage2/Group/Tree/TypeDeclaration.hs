module Stage2.Group.Tree.TypeDeclaration where

import qualified Graph.StronglyConnected as StronglyConnected
import Stage1.Position (Position)
import Stage1.Variable (ConstructorIdentifier)
import Stage2.Group.Tree.TypeGroup (TypeGroup)
import qualified Stage2.Group.Tree.TypeGroup as TypeGroup
import qualified Stage2.Index.Type0 as Type0
import Stage2.Tree.Type (Type)
import qualified Stage2.Tree.TypeDeclaration as Proper
import Stage2.Tree.TypeDefinition (TypeDefinition)

data TypeDeclaration scope
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
        meta :: !(TypeGroup scope)
      }
  deriving (Show)

group ::
  (Type0.Index scope -> Proper.TypeDeclaration scope) ->
  StronglyConnected.Component (Type0.Index scope) ->
  Proper.TypeDeclaration scope ->
  TypeDeclaration scope
group _ _ Proper.Annotated {position, name, annotation, definition} =
  Annotated {position, name, annotation, definition}
group index component Proper.Inferred {position, name, definition} =
  Inferred
    { position,
      name,
      definition,
      meta = TypeGroup.group index component
    }

proper :: TypeDeclaration scope -> Proper.TypeDeclaration scope
proper = \case
  Annotated {position, name, annotation, definition} ->
    Proper.Annotated {position, name, annotation, definition}
  Inferred {position, name, definition} ->
    Proper.Inferred {position, name, definition}
