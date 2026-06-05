module Semantic.Resolve.Binding.Term (Binding (..), Selector (..), fromFunctor, toFunctor) where

import Error (duplicateVariableEntries)
import qualified Semantic.Index.Term2 as Term2
import Semantic.Resolve.Detail.Binding.Term (Selector (..))
import qualified Semantic.Resolve.Detail.Binding.Term as Detail
import qualified Semantic.Resolve.Functor.Binding.Term as Functor
import Semantic.Shift (Shift (..), shiftDefault)
import qualified Semantic.Shift as Shift
import Syntax.Position (Position)
import Syntax.Tree.Fixity (Fixity)

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
