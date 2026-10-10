module Core.Tree.Constructor where

import qualified Core.Substitute as Substitute
import Core.Tree.Entry (EntryF)
import qualified Core.Tree.Entry as Entry
import qualified Data.Vector.Strict as Strict
import Data.Void (Void)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Stage (Check)
import Semantic.Tree.Constructor (Syntax)
import qualified Semantic.Tree.Constructor as Solved
import qualified Syntax.Variable as Variable

type Constructor = ConstructorF Void

data ConstructorF logical scope = Constructor
  { name :: !Variable.Constructor,
    syntax :: !Syntax,
    entries :: !(Strict.Vector (EntryF logical scope))
  }
  deriving (Show)

instance Shift0.Functor (ConstructorF logical) where
  map = Shift.mapDefault

instance Shift.Functor (ConstructorF logical) where
  map category Constructor {name, syntax, entries} =
    Constructor
      { name,
        syntax,
        entries = Shift.map category <$> entries
      }

instance (logical ~ Void) => Substitute.Functor (ConstructorF logical) where
  map category Constructor {name, syntax, entries} =
    Constructor
      { name,
        syntax,
        entries = Substitute.map category <$> entries
      }

simplify :: Solved.Constructor Check scope -> Constructor scope
simplify = \case
  Solved.Constructor {name, syntax, entries} ->
    Constructor
      { name,
        syntax,
        entries = Entry.simplify <$> entries
      }
