module Stage3.Tree.SelectorInfo where

import qualified Data.Strict.Maybe as Strict (Maybe)
import qualified Data.Vector.Strict as Strict (Vector)
import Stage2.Shift (Shift (..), shiftDefault)
import qualified Stage2.Shift as Shift
import Stage3.Tree.ConstructorInfo (ConstructorInfo)
import qualified Stage4.Shift as Shift2
import qualified Stage4.Tree.Type as Simple

data SelectorInfo scope
  = Uniform
      { strict :: !(Simple.Type scope)
      }
  | Disjoint
      { select :: !(Strict.Vector (Select scope))
      }
  deriving (Show)

data Select scope
  = Select
  { selectIndex :: !(Strict.Maybe Int),
    constructorInfo :: !(ConstructorInfo scope)
  }
  deriving (Show)

instance Shift SelectorInfo where
  shift = shiftDefault

instance Shift.Functor SelectorInfo where
  map = Shift2.mapDefault

instance Shift2.Functor SelectorInfo where
  map category = \case
    Uniform {strict} -> Uniform {strict = Shift2.map category strict}
    Disjoint {select} -> Disjoint {select = Shift2.map category <$> select}

instance Shift Select where
  shift = shiftDefault

instance Shift.Functor Select where
  map = Shift2.mapDefault

instance Shift2.Functor Select where
  map category Select {selectIndex, constructorInfo} =
    Select
      { selectIndex,
        constructorInfo = Shift2.map category constructorInfo
      }
