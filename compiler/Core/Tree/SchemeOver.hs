module Core.Tree.SchemeOver where

import qualified Core.Shift as Shift2
import qualified Core.Substitute as Substitute
import Core.Tree.Constraints (ConstraintCount, Constraints)
import qualified Core.Tree.Constraints as Constraints
import Core.Tree.Type (TypeF)
import qualified Data.Kind
import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector
import Semantic.Scope (Environment (..), IsVacuous, Local, Vacuous)
import qualified Semantic.Scope as Scope
import Semantic.Shift (Shift (..), shiftDefault)
import qualified Semantic.Shift as Shift

type SchemeOver = SchemeOverF Vacuous

data SchemeOverF logical typex scope = SchemeOver
  { parameters :: !(Strict.Vector (TypeF logical scope)),
    constraints :: !(Constraints scope),
    result :: !(typex (Local ':+ scope))
  }

instance (Scope.Show logical, Scope.Show typex) => Scope.Show (SchemeOverF logical typex) where
  showsPrec = showsPrec

instance (Scope.Show logical, Scope.Show typex) => Show (SchemeOverF logical typex scope) where
  showsPrec _ SchemeOver {parameters, constraints, result} =
    foldr
      (.)
      id
      [ showString "SchemeOver { parameters = ",
        shows parameters,
        showString ", constraints = ",
        shows constraints,
        showString ", result = ",
        Scope.shows result,
        showString " }"
      ]

instance (Shift.Functor logical, Shift.Functor typex) => Shift (SchemeOverF logical typex) where
  shift = shiftDefault

instance (Shift.Functor logical, Shift.Functor typex) => Shift.Functor (SchemeOverF logical typex) where
  map category SchemeOver {parameters, constraints, result} =
    SchemeOver
      { parameters = fmap (Shift.map category) parameters,
        constraints = Shift.map category constraints,
        result = Shift.map (Shift.Over category) result
      }

instance
  (IsVacuous logical, Shift.Functor logical, Shift2.Functor typex) =>
  Shift2.Functor (SchemeOverF logical typex)
  where
  map category SchemeOver {parameters, constraints, result} =
    SchemeOver
      { parameters = fmap (Shift2.map category) parameters,
        constraints = Shift2.map category constraints,
        result = Shift2.map (Shift2.Over category) result
      }

instance
  (IsVacuous logical, Shift.Functor logical, Substitute.Functor typex) =>
  Substitute.Functor (SchemeOverF logical typex)
  where
  map category SchemeOver {parameters, constraints, result} =
    SchemeOver
      { parameters = fmap (Substitute.map category) parameters,
        constraints = Substitute.map category constraints,
        result = Substitute.map (Substitute.Over category) result
      }

mono :: (Shift typex) => typex scope -> SchemeOver typex scope
mono result =
  SchemeOver
    { parameters = Strict.Vector.empty,
      constraints = Constraints.None,
      result = shift result
    }

constraintCount :: SchemeOver typex scope -> ConstraintCount
constraintCount SchemeOver {constraints} = Constraints.constraintCount constraints

type Map :: (Environment -> Data.Kind.Type) -> (Environment -> Data.Kind.Type) -> Data.Kind.Type
newtype Map typex typex' = Map (forall scope. typex scope -> typex' scope)

map :: Map typex typex' -> SchemeOver typex scope -> SchemeOver typex' scope
map (Map map) SchemeOver {parameters, constraints, result} =
  SchemeOver
    { parameters,
      constraints,
      result = map result
    }
