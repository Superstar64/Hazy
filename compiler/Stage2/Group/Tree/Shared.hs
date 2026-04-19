module Stage2.Group.Tree.Shared where

import qualified Graph.StronglyConnected as StronglyConnected
import Stage1.Position (Position)
import qualified Stage2.Group.Index.Term0 as Term0
import qualified Stage2.Group.Temporary.Declaration as Temporary
import Stage2.Group.Tree.Group (Group)
import qualified Stage2.Group.Tree.Group as Group
import Stage2.Tree.Pattern (Pattern)
import Stage2.Tree.RightHandSide (RightHandSide)
import qualified Stage2.Tree.Shared as Proper

data Shared scope = Shared
  { equalPosition :: !Position,
    patternx :: !(Pattern scope),
    definition :: !(RightHandSide scope),
    meta :: !(Group scope)
  }
  deriving (Show)

group ::
  (Term0.Index scope -> Temporary.Declaration scope) ->
  StronglyConnected.Component (Term0.Index scope) ->
  Proper.Shared scope ->
  Shared scope
group index component Proper.Shared {equalPosition, patternx, definition} =
  Shared
    { equalPosition,
      patternx,
      definition,
      meta = Group.group index component
    }

proper :: Shared scope -> Proper.Shared scope
proper = \case
  Shared {equalPosition, patternx, definition} ->
    Proper.Shared {equalPosition, patternx, definition}
