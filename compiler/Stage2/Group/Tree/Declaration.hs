module Stage2.Group.Tree.Declaration where

import qualified Graph.StronglyConnected as StronglyConnected
import Stage1.Position (Position)
import Stage1.Tree.Fixity (Fixity)
import Stage1.Variable (Variable)
import qualified Stage2.Group.Index.Link.Term as Term
import qualified Stage2.Group.Temporary.Declaration as Temporary (Declaration (..))
import Stage2.Group.Tree.Group (Group)
import qualified Stage2.Group.Tree.Group as Group
import qualified Stage2.Tree.Declaration as Proper
import Stage2.Tree.Definition2 (Annotated, Definition2, Inferred)
import Stage2.Tree.Scheme (Scheme)

data Declaration locality scope
  = Annotated
      { position :: !Position,
        name :: !Variable,
        fixity :: !Fixity,
        annotation :: !(Scheme Position scope),
        definition :: !(Definition2 Annotated scope)
      }
  | Inferred
      { position :: !Position,
        name :: !Variable,
        fixity :: !Fixity,
        definition' :: !(Definition2 Inferred scope),
        meta :: !(Group locality scope)
      }
  deriving (Show)

group ::
  (Term.Link locality -> Temporary.Declaration scope) ->
  StronglyConnected.Component (Term.Link locality) ->
  Proper.Declaration scope ->
  Declaration locality scope
group _ _ Proper.Annotated {position, name, fixity, annotation, definition} =
  Annotated {position, name, fixity, annotation, definition}
group index component Proper.Inferred {position, name, fixity, definition'} =
  Inferred
    { position,
      name,
      fixity,
      definition',
      meta = Group.group index component
    }

proper :: Declaration locality scope -> Proper.Declaration scope
proper = \case
  Annotated {position, name, fixity, annotation, definition} ->
    Proper.Annotated {position, name, fixity, annotation, definition}
  Inferred {position, name, fixity, definition'} ->
    Proper.Inferred {position, name, fixity, definition'}
