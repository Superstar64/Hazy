module Stage2.Resolve.Binding.Constructor where

import Data.Map (Map)
import qualified Data.Strict.Maybe as Strict (Maybe)
import qualified Data.Vector.Strict as Strict (Vector)
import Error (duplicateConstructorEntries)
import Stage1.Position (Position)
import Stage1.Tree.Fixity (Fixity)
import Stage1.Variable (Variable)
import qualified Stage2.Index.Constructor as Constructor
import qualified Stage2.Resolve.Detail.Binding.Constructor as Detail
import qualified Stage2.Resolve.Functor.Binding.Constructor as Functor
import Stage2.Shift (Shift (..), shiftDefault)
import qualified Stage2.Shift as Shift

data Binding scope = Binding
  { position :: !Position,
    index :: !(Constructor.Index scope),
    fixity :: !Fixity,
    fields :: !(Map Variable Int),
    -- redirect a selector index into an entry index
    selections :: !(Strict.Vector (Strict.Maybe Int)),
    unordered :: !Bool,
    fielded :: !Bool
  }
  deriving (Show)

instance Semigroup (Binding scope) where
  binding@Binding {index = index1, position = position1}
    <> Binding {index = index2, position = position2}
      | index1 == index2 = binding
      | otherwise = duplicateConstructorEntries [position1, position2]

instance Shift Binding where
  shift = shiftDefault

instance Shift.Functor Binding where
  map category Binding {position, index, fixity, fields, selections, unordered, fielded} =
    Binding
      { position,
        index = Shift.map category index,
        fixity,
        fields,
        selections,
        unordered,
        fielded
      }

fromFunctor :: Functor.Binding (Detail.Binding scope) -> Binding scope
fromFunctor
  Functor.Binding
    { Functor.position,
      Functor.value =
        Detail.Binding
          { Detail.index,
            Detail.fixity,
            Detail.fields,
            Detail.selections,
            Detail.unordered,
            Detail.fielded
          }
    } =
    Binding
      { position,
        index,
        fixity,
        fields,
        selections,
        unordered,
        fielded
      }

toFunctor :: Binding scope -> Functor.Binding (Detail.Binding scope)
toFunctor
  Binding
    { position,
      index,
      fixity,
      fields,
      selections,
      unordered,
      fielded
    } =
    Functor.Binding
      { Functor.position,
        Functor.value =
          Detail.Binding
            { Detail.index,
              Detail.fixity,
              Detail.fields,
              Detail.selections,
              Detail.unordered,
              Detail.fielded
            }
      }
