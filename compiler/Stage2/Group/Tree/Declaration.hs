module Stage2.Group.Tree.Declaration where

import qualified Data.Set as Set
import qualified Graph.StronglyConnected as StronglyConnected
import Stage1.Position (Position)
import Stage1.Tree.Fixity (Fixity)
import Stage1.Variable (Variable)
import qualified Stage2.Group.Index.Term0 as Term0
import qualified Stage2.Group.Temporary.Declaration as Temporary (Declaration (..))
import Stage2.Group.Tree.Group (Group)
import qualified Stage2.Group.Tree.Group as Group
import qualified Stage2.Tree.Declaration as Proper
import Stage2.Tree.Definition2 (Annotated, Definition2)
import Stage2.Tree.Scheme (Scheme)

data Declaration scope
  = Annotated
      { position :: !Position,
        name :: !Variable,
        fixity :: !Fixity,
        annotation :: !(Scheme Position scope),
        definition :: !(Definition2 Annotated scope)
      }
  | Group !(Group scope)
  deriving (Show)

group ::
  (Term0.Index scope -> Temporary.Declaration scope) ->
  StronglyConnected.Component (Term0.Index scope) ->
  Declaration scope
group index = \case
  StronglyConnected.Group {set}
    | [single] <- Set.toList set,
      Temporary.Declaration
        Proper.Annotated
          { position,
            name,
            fixity,
            annotation,
            definition
          } <-
        index single ->
        Annotated {position, name, fixity, annotation, definition}
  component -> Group (Group.group index component)
