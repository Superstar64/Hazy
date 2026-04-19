module Stage2.Group.Tree.TypeDeclaration where

import qualified Data.Set as Set
import qualified Data.Strict.Maybe as Strict
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
  | Group !(TypeGroup scope)
  deriving (Show)

group ::
  (Type0.Index scope -> Proper.TypeDeclaration scope) ->
  StronglyConnected.Component (Type0.Index scope) ->
  TypeDeclaration scope
group index = \case
  StronglyConnected.Group {set}
    | [single] <- Set.toList set,
      Proper.TypeDeclaration
        { position,
          name,
          annotation = Strict.Just annotation,
          definition
        } <-
        index single ->
        Annotated {position, name, annotation, definition}
  component -> Group $ TypeGroup.group index component
