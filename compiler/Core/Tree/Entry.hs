module Core.Tree.Entry where

import qualified Core.Substitute as Substitute
import Core.Tree.Type (Type)
import qualified Core.Tree.Type as Type
import qualified Semantic.Index.Type2 as Type2
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Stage (Check)
import Semantic.Tree.Entry (Restricted (..))
import qualified Semantic.Tree.Entry as Solved
import Semantic.Tree.StrictnessAnnotation (StrictnessAnnotation (..))

data Entry scope = Entry
  { entry :: !(Type scope),
    strict :: !(Type scope)
  }
  deriving (Show)

instance Shift0.Functor Entry where
  map = Shift.mapDefault

instance Shift.Functor Entry where
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
