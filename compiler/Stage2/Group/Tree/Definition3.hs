module Stage2.Group.Tree.Definition3 where

import Stage1.Position (Position)
import Stage1.Tree.Fixity (Fixity)
import Stage1.Variable (Variable)
import qualified Stage2.Group.Temporary.Declaration as Temporary
import qualified Stage2.Tree.Declaration as Proper (Declaration (..))
import Stage2.Tree.Definition2 (Definition2, Inferred)
import Stage2.Tree.Pattern (Pattern)
import Stage2.Tree.RightHandSide (RightHandSide)
import qualified Stage2.Tree.Shared as Proper (Shared (..))

data Definition3 locality scope
  = Inferred
      { position :: !Position,
        name :: !Variable,
        fixity :: !Fixity,
        definition' :: !(Definition2 locality Inferred scope)
      }
  | Shared
      { equalPosition :: !Position,
        patternx :: !(Pattern scope),
        definition :: !(RightHandSide scope)
      }
  deriving (Show)

group :: Temporary.Declaration locality scope -> Definition3 locality scope
group = \case
  Temporary.Declaration Proper.Inferred {position, name, fixity, definition'} ->
    Inferred
      { position,
        name,
        fixity,
        definition'
      }
  Temporary.Shared Proper.Shared {equalPosition, patternx, definition} ->
    Shared
      { equalPosition,
        patternx,
        definition
      }
  _ -> error "bad group declaration"
