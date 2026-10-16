module Core.Tree.Entry where

import Core.Instanciate (Normal, Store (Empty))
import qualified Core.Substitute as Substitute
import Core.Tree.Type (TypeF)
import qualified Core.Tree.Type as Type
import Data.Void (Void)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Stage (Check)
import Semantic.Tree.Entry (Restricted (..))
import qualified Semantic.Tree.Entry as Solved
import Semantic.Tree.StrictnessAnnotation (StrictnessAnnotation (..))
import Syntax.Position (Position)

type Entry = EntryF Normal Void

data EntryF instanciate logical scope = Entry
  { position :: !(Store instanciate Position),
    entry :: !(TypeF logical scope),
    levity :: !(TypeF logical scope)
  }
  deriving (Show)

instance Shift0.Functor (EntryF instanciate logical) where
  map = Shift.mapDefault

instance Shift.Functor (EntryF instanciate logical) where
  map category Entry {position, entry, levity} =
    Entry
      { position,
        entry = Shift.map category entry,
        levity = Shift.map category levity
      }

instance (logical ~ Void) => Substitute.Functor (EntryF instanciate logical) where
  map category Entry {position, entry, levity} =
    Entry
      { position,
        entry = Substitute.map category entry,
        levity = Substitute.map category levity
      }

simplify :: Solved.Entry position Check scope -> Entry scope
simplify Solved.Entry {entry = Restricted entry, strict} =
  Entry
    { position = Empty,
      entry = Type.simplify entry,
      levity = case strict of
        Lazy -> Type.lazy
        Strict -> Type.strict
        Polymorphic {levity} -> Type.simplify levity
    }
