module Core.Tree.Forall where

import qualified Core.Shift as Shift2
import qualified Core.Show as Core
import qualified Core.Substitute as Substitute
import Core.Tree.Constraints (ConstraintsF)
import qualified Core.Tree.Constraints as Constraints
import Core.Tree.Type (TypeF)
import qualified Core.Tree.Type as Type
import qualified Data.Kind as Kind
import qualified Data.Vector.Strict as Strict
import Semantic.Scope (Environment (..), Local, Vacuous)
import qualified Semantic.Scope as Scope
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Stage (Check)
import qualified Semantic.Tree.Scheme as Solved
import qualified Semantic.Tree.TypePattern as Solved.TypePattern

type Forall = ForallOver TypeF Vacuous

data ForallOver typef logical scope = ForallOver
  { parameters :: !(Strict.Vector (TypeF logical scope)),
    constraints :: !(ConstraintsF logical scope),
    result :: !(typef logical (Local ':+ scope))
  }

instance (Scope.Show logical, Core.Show typef) => Scope.Show (ForallOver typef logical) where
  showsPrec = showsPrec

instance (Scope.Show logical, Core.Show typef) => Show (ForallOver typef logical scope) where
  showsPrec _ ForallOver {parameters, constraints, result} =
    foldr
      (.)
      id
      [ showString "ForallOver { parameters = ",
        shows parameters,
        showString ", constraints = ",
        shows constraints,
        showString ", result = ",
        Core.showsPrec 0 result,
        showString " }"
      ]

instance (Shift.Functor logical, Shift.Functor (typef logical)) => Shift0.Functor (ForallOver typef logical) where
  map = Shift.mapDefault

instance (Shift.Functor logical, Shift.Functor (typef logical)) => Shift.Functor (ForallOver typef logical) where
  map category ForallOver {parameters, constraints, result} =
    ForallOver
      { parameters = Shift.map category <$> parameters,
        constraints = Shift.map category constraints,
        result = Shift.map (Shift.Over category) result
      }

instance
  (logical ~ Vacuous, Substitute.TypeFunctor typef, Shift.Functor (typef Vacuous)) =>
  Shift2.Functor (ForallOver typef logical)
  where
  map = Substitute.mapDefault

instance
  (logical ~ Vacuous, Substitute.TypeFunctor typef, Shift.Functor (typef Vacuous)) =>
  Substitute.Functor (ForallOver typef logical)
  where
  map = Substitute.mapType

instance (Substitute.TypeFunctor typex) => Substitute.TypeFunctor (ForallOver typex) where
  mapType category ForallOver {parameters, constraints, result} =
    ForallOver
      { parameters = Substitute.mapType category <$> parameters,
        constraints = Substitute.mapType category constraints,
        result = Substitute.mapType (Substitute.Over category) result
      }

constraintCount :: ForallOver typef Vacuous scope -> Constraints.ConstraintCount
constraintCount ForallOver {constraints} = Constraints.constraintCount constraints

simplify :: Solved.Scheme position Check scope -> Forall scope
simplify Solved.Scheme {parameters, constraints, result}
  | parameters <- fmap Solved.TypePattern.typex' parameters,
    constraints <- Constraints.simplify constraints,
    result <- Type.simplify result =
      ForallOver
        { parameters,
          constraints,
          result
        }

type Map ::
  (Environment -> Kind.Type) ->
  (((Environment -> Kind.Type)) -> Environment -> Kind.Type) ->
  (((Environment -> Kind.Type)) -> Environment -> Kind.Type) ->
  Kind.Type
newtype Map logical typef typef' = Map (forall scope. typef logical scope -> typef' logical scope)

map :: Map logical typef typef' -> ForallOver typef logical scope -> ForallOver typef' logical scope
map (Map map) ForallOver {parameters, constraints, result} =
  ForallOver
    { parameters,
      constraints,
      result = map result
    }
