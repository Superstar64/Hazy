module Stage4.Tree.Constructor where

import qualified Data.Vector.Strict as Strict
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Stage (Check)
import qualified Stage2.Tree.Constructor as Solved
import qualified Stage2.Tree.Field as Solved.Field
import qualified Stage4.Shift as Shift2
import qualified Stage4.Substitute as Substitute
import Stage4.Tree.Entry (Entry)
import qualified Stage4.Tree.Entry as Entry

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
