module Semantic.Check.Simple.SelectorInfo where

import qualified Core.Tree.Type as Simple
import qualified Data.Strict.Maybe as Strict (Maybe)
import qualified Data.Vector.Strict as Strict (Vector)
import Semantic.Check.Simple.ConstructorInfo (ConstructorInfo)
import qualified Semantic.Scope as Scope
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0

data SelectorInfo scope
  = Uniform
      { strict :: !(Simple.Type scope)
      }
  | Disjoint
      { select :: !(Strict.Vector (Select scope))
      }
  deriving (Show)

instance Scope.Show SelectorInfo where
  showsPrec = showsPrec

data Select scope
  = Select
  { selectIndex :: !(Strict.Maybe Int),
    constructorInfo :: !(ConstructorInfo scope)
  }
  deriving (Show)

instance Shift0.Functor SelectorInfo where
  map = Shift.mapDefault

instance Shift.Functor SelectorInfo where
  map category = \case
    Uniform {strict} -> Uniform {strict = Shift.map category strict}
    Disjoint {select} -> Disjoint {select = Shift.map category <$> select}

instance Shift0.Functor Select where
  map = Shift.mapDefault

instance Shift.Functor Select where
  map category Select {selectIndex, constructorInfo} =
    Select
      { selectIndex,
        constructorInfo = Shift.map category constructorInfo
      }
