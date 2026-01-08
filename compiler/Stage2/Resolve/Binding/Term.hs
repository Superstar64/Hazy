module Stage2.Resolve.Binding.Term (Binding (..), Selector (..), fromFunctor, toFunctor) where

import Error (duplicateVariableEntries)
import Stage1.Position (Position)
import Stage1.Tree.Fixity (Fixity)
import qualified Stage2.Index.Term2 as Term2
import Stage2.Resolve.Detail.Binding.Term (Selector (..))
import qualified Stage2.Resolve.Detail.Binding.Term as Detail
import qualified Stage2.Resolve.Functor.Binding.Term as Functor
import Stage2.Shift (Shift (..), shiftDefault)
import qualified Stage2.Shift as Shift

data Binding scope = Binding
  { position :: !Position,
    index :: !(Term2.Index scope),
    fixity :: !Fixity,
    selector :: Selector scope
  }
  deriving (Show)

instance Semigroup (Binding scope) where
  binding@Binding {index = index1, position = position1}
    <> Binding {index = index2, position = position2}
      | index1 == index2 = binding
      | otherwise = duplicateVariableEntries [position1, position2]

instance Shift Binding where
  shift = shiftDefault

instance Shift.Functor Binding where
  map category Binding {position, index, fixity, selector} =
    Binding
      { position,
        index = Shift.map category index,
        fixity,
        selector = Shift.map category selector
      }

fromFunctor :: Functor.Binding (Detail.Binding scope) -> Binding scope
fromFunctor
  Functor.Binding
    { position,
      value =
        Detail.Binding
          { index,
            fixity,
            selector
          }
    } =
    Binding
      { position,
        index,
        fixity,
        selector
      }

toFunctor :: Binding scope -> Functor.Binding (Detail.Binding scope)
toFunctor
  Binding
    { position,
      index,
      fixity,
      selector
    } =
    Functor.Binding
      { position,
        value =
          Detail.Binding
            { index,
              fixity,
              selector
            }
      }
