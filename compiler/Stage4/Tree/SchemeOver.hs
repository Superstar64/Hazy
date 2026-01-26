module Stage4.Tree.SchemeOver where

import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector
import Stage2.Scope (Environment (..), Local)
import qualified Stage2.Scope as Scope
import Stage2.Shift (Shift (..), shiftDefault)
import qualified Stage2.Shift as Shift
import qualified Stage4.Shift as Shift2
import qualified Stage4.Substitute as Substitute
import Stage4.Tree.Constraint (Constraint)
import Stage4.Tree.Type (Type)

data SchemeOver typex scope = SchemeOver
  { parameters :: !(Strict.Vector (Type scope)),
    constraints :: !(Strict.Vector (Constraint scope)),
    result :: !(typex (Local ':+ scope))
  }

instance (Scope.Show typex) => Show (SchemeOver typex scope) where
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

instance (Shift.Functor typex) => Shift (SchemeOver typex) where
  shift = shiftDefault

instance (Shift.Functor typex) => Shift.Functor (SchemeOver typex) where
  map category SchemeOver {parameters, constraints, result} =
    SchemeOver
      { parameters = fmap (Shift.map category) parameters,
        constraints = fmap (Shift.map category) constraints,
        result = Shift.map (Shift.Over category) result
      }

instance (Shift2.Functor typex) => Shift2.Functor (SchemeOver typex) where
  map category SchemeOver {parameters, constraints, result} =
    SchemeOver
      { parameters = fmap (Shift2.map category) parameters,
        constraints = fmap (Shift2.map category) constraints,
        result = Shift2.map (Shift2.Over category) result
      }

instance (Substitute.Functor typex) => Substitute.Functor (SchemeOver typex) where
  map category SchemeOver {parameters, constraints, result} =
    SchemeOver
      { parameters = fmap (Substitute.map category) parameters,
        constraints = fmap (Substitute.map category) constraints,
        result = Substitute.map (Substitute.Over category) result
      }

mono :: (Shift typex) => typex scope -> SchemeOver typex scope
mono result =
  SchemeOver
    { parameters = Strict.Vector.empty,
      constraints = Strict.Vector.empty,
      result = shift result
    }

constraintCount :: SchemeOver typex scope -> Int
constraintCount SchemeOver {constraints} = length constraints
