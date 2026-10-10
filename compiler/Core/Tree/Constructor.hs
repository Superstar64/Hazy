module Core.Tree.Constructor where

import Core.Instanciate (Normal, Store (Empty))
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
import Syntax.Tree.Brand (Brand)
import qualified Syntax.Variable as Variable

type Constructor = ConstructorF Normal Void

data ConstructorF instanciate logical scope = Constructor
  { brand :: !(Store instanciate Brand),
    name :: !Variable.Constructor,
    syntax :: !Syntax,
    entries :: !(Strict.Vector (EntryF instanciate logical scope))
  }
  deriving (Show)

instance Shift0.Functor (ConstructorF instanciate logical) where
  map = Shift.mapDefault

instance Shift.Functor (ConstructorF instanciate logical) where
  map category Constructor {brand, name, syntax, entries} =
    Constructor
      { brand,
        name,
        syntax,
        entries = Shift.map category <$> entries
      }

instance (logical ~ Void) => Substitute.Functor (ConstructorF instanciate logical) where
  map category Constructor {brand, name, syntax, entries} =
    Constructor
      { brand,
        name,
        syntax,
        entries = Substitute.map category <$> entries
      }

simplify :: Solved.Constructor Check scope -> Constructor scope
simplify = \case
  Solved.Constructor {name, syntax, entries} ->
    Constructor
      { brand = Empty,
        name,
        syntax,
        entries = Entry.simplify <$> entries
      }
