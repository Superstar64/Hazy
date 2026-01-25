module Stage4.Tree.Constructor where

import qualified Data.Vector.Strict as Strict
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import qualified Stage3.Tree.Constructor as Solved
import qualified Stage3.Tree.Entry as Solved.Entry
import qualified Stage3.Tree.Field as Solved.Field
import qualified Stage4.Shift as Shift2
import Stage4.Tree.Type (Type)

newtype Constructor scope = Constructor
  { entries :: Strict.Vector (Type scope)
  }
  deriving (Show)

instance Shift Constructor where
  shift = shiftDefault

instance Shift.Functor Constructor where
  map = Shift2.mapDefault

instance Shift2.Functor Constructor where
  map category Constructor {entries} =
    Constructor
      { entries = Shift2.map category <$> entries
      }

simplify :: Solved.Constructor scope -> Constructor scope
simplify = \case
  Solved.Constructor {entries} ->
    Constructor
      { entries = Solved.Entry.entry' <$> entries
      }
  Solved.Record {fields} ->
    Constructor
      { entries = Solved.Entry.entry' . Solved.Field.entry <$> fields
      }
