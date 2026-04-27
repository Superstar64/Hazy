module Stage2.Group.Tree.Shared where

import qualified Graph.StronglyConnected as StronglyConnected
import Stage1.Position (Position)
import qualified Stage2.Group.Index.Link.Term as Term
import qualified Stage2.Group.Temporary.Declaration as Temporary
import Stage2.Group.Tree.Group (Group)
import qualified Stage2.Group.Tree.Group as Group
import Stage2.Tree.Pattern (Pattern)
import Stage2.Tree.RightHandSide (RightHandSide)
import qualified Stage2.Tree.Shared as Proper

data Shared locality scope = Shared
  { equalPosition :: !Position,
    patternx :: !(Pattern scope),
    definition :: !(RightHandSide scope),
    meta :: !(Group locality scope)
  }
  deriving (Show)

group ::
  (Term.Link locality -> Temporary.Declaration scope) ->
  StronglyConnected.Component (Term.Link locality) ->
  Proper.Shared scope ->
  Shared locality scope
group index component Proper.Shared {equalPosition, patternx, definition} =
  Shared
    { equalPosition,
      patternx,
      definition,
      meta = Group.group index component
    }

proper :: Shared locality scope -> Proper.Shared scope
proper = \case
  Shared {equalPosition, patternx, definition} ->
    Proper.Shared {equalPosition, patternx, definition}
