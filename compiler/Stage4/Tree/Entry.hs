module Stage4.Tree.Entry where

import qualified Stage2.Index.Type2 as Type2
import Stage2.Shift (Shift (..), shiftDefault)
import qualified Stage2.Shift as Shift
import qualified Stage3.Tree.Entry as Solved
import Stage3.Tree.StrictnessAnnotation (StrictnessAnnotation (..))
import qualified Stage4.Shift as Shift2
import qualified Stage4.Substitute as Substitute
import Stage4.Tree.Type (Type)
import qualified Stage4.Tree.Type as Type

data Entry scope = Entry
  { entry :: !(Type scope),
    strict :: !(Type scope)
  }
  deriving (Show)

instance Shift Entry where
  shift = shiftDefault

instance Shift.Functor Entry where
  map = Shift2.mapDefault

instance Shift2.Functor Entry where
  map = Substitute.mapDefault

instance Substitute.Functor Entry where
  map category Entry {entry, strict} =
    Entry
      { entry = Substitute.map category entry,
        strict = Substitute.map category strict
      }

simplify :: Solved.Entry scope -> Entry scope
simplify Solved.Entry {entry, strict} =
  Entry
    { entry = Type.simplify entry,
      strict = case strict of
        Lazy -> Type.Constructor Type2.Lazy
        Strict -> Type.Constructor Type2.Strict
        Polymorphic {levity} -> Type.simplify levity
    }
