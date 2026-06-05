module Core.Tree.Constructor where

import qualified Core.Shift as Shift2
import qualified Core.Substitute as Substitute
import Core.Tree.Entry (Entry)
import qualified Core.Tree.Entry as Entry
import qualified Data.Vector.Strict as Strict
import Semantic.Shift (Shift, shiftDefault)
import qualified Semantic.Shift as Shift
import Semantic.Stage (Check)
import qualified Semantic.Tree.Constructor as Solved
import qualified Semantic.Tree.Field as Solved.Field

newtype Constructor scope = Constructor
  { entries :: Strict.Vector (Entry scope)
  }
  deriving (Show)

instance Shift Constructor where
  shift = shiftDefault

instance Shift.Functor Constructor where
  map = Shift2.mapDefault

instance Shift2.Functor Constructor where
  map = Substitute.mapDefault

instance Substitute.Functor Constructor where
  map category Constructor {entries} =
    Constructor
      { entries = Substitute.map category <$> entries
      }

simplify :: Solved.Constructor Check scope -> Constructor scope
simplify = \case
  Solved.Constructor {entries} ->
    Constructor
      { entries = Entry.simplify <$> entries
      }
  Solved.Record {fields} ->
    Constructor
      { entries = Entry.simplify . Solved.Field.entry <$> fields
      }
