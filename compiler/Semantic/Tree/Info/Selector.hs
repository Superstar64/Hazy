module Semantic.Tree.Info.Selector where

import qualified Core.Tree.Type as Simple
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Semantic.Scope as Scope
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0

data SelectorInfo scope
  = Newtype
  | Uniform
      { levity :: !(Simple.Type scope)
      }
  | Disjoint
      { select :: !(Strict.Vector (Select scope))
      }
  deriving (Show)

instance Scope.Show SelectorInfo where
  showsPrec = showsPrec

data Select scope
  = Select
      { entryCount :: !Int,
        specificLevity :: !(Simple.Type scope),
        selection :: !Int
      }
  | Bottom
      { entryCount :: !Int
      }
  deriving (Show)

instance Shift0.Functor SelectorInfo where
  map = Shift.mapDefault

instance Shift.Functor SelectorInfo where
  map category = \case
    Newtype -> Newtype
    Uniform {levity} -> Uniform {levity = Shift.map category levity}
    Disjoint {select} -> Disjoint {select = Shift.map category <$> select}

instance Shift0.Functor Select where
  map = Shift.mapDefault

instance Shift.Functor Select where
  map category = \case
    Select {entryCount, specificLevity, selection} ->
      Select
        { entryCount,
          specificLevity = Shift.map category specificLevity,
          selection
        }
    Bottom {entryCount} -> Bottom {entryCount}
