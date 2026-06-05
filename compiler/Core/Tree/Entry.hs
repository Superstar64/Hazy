module Core.Tree.Entry where

import qualified Core.Shift as Shift2
import qualified Core.Substitute as Substitute
import Core.Tree.Type (Type)
import qualified Core.Tree.Type as Type
import qualified Semantic.Index.Type2 as Type2
import Semantic.Shift (Shift (..), shiftDefault)
import qualified Semantic.Shift as Shift
import Semantic.Stage (Check)
import Semantic.Tree.Entry (Restricted (..))
import qualified Semantic.Tree.Entry as Solved
import Semantic.Tree.StrictnessAnnotation (StrictnessAnnotation (..))

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

simplify :: Solved.Entry position Check scope -> Entry scope
simplify Solved.Entry {entry = Restricted entry, strict} =
  Entry
    { entry = Type.simplify entry,
      strict = case strict of
        Lazy -> Type.Constructor Type2.Lazy
        Strict -> Type.Constructor Type2.Strict
        Polymorphic {levity} -> Type.simplify levity
    }
