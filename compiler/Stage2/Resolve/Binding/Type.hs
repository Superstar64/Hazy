module Stage2.Resolve.Binding.Type where

import Data.Map (Map)
import Data.Set (Set)
import Error (duplicateTypeEntries)
import Stage1.Position (Position)
import Stage1.Variable (Constructor, Variable)
import qualified Stage2.Index.Type3 as Type3
import qualified Stage2.Resolve.Detail.Binding.Type as Detail
import qualified Stage2.Resolve.Functor.Binding.Type as Functor
import Stage2.Shift (Shift (..), shiftDefault)
import qualified Stage2.Shift as Shift

data Binding scope = Binding
  { position :: !Position,
    index :: !(Type3.Index scope),
    methods :: !(Map Variable Int),
    constructors :: !(Set Constructor),
    fields :: !(Set Variable)
  }
  deriving (Show)

instance Semigroup (Binding scope) where
  binding@Binding {index = index1, position = position1}
    <> Binding {index = index2, position = position2}
      | index1 == index2 = binding
      | otherwise = duplicateTypeEntries [position1, position2]

instance Shift Binding where
  shift = shiftDefault

instance Shift.Functor Binding where
  map category Binding {position, index, methods, constructors, fields} =
    Binding
      { position,
        index = Shift.map category index,
        methods,
        constructors,
        fields
      }

fromFunctor :: Functor.Binding (Detail.Binding scope) -> Binding scope
fromFunctor
  Functor.Binding
    { position,
      fields,
      constructors,
      value =
        Detail.Binding
          { index,
            methods
          }
    } =
    Binding
      { position,
        fields,
        constructors,
        index,
        methods
      }

toFunctor :: Binding scope -> Functor.Binding (Detail.Binding scope)
toFunctor
  Binding
    { position,
      fields,
      constructors,
      index,
      methods
    } =
    Functor.Binding
      { position,
        fields,
        constructors,
        value =
          Detail.Binding
            { index,
              methods
            }
      }
