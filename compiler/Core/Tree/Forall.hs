module Core.Tree.Forall where

import qualified Core.Functor as Core (Functor (..))
import qualified Core.Show as Core (Show (..))
import qualified Core.Substitute as Substitute
import Core.Tree.Constraints (ConstraintsF (None))
import qualified Core.Tree.Constraints as Constraints
import Core.Tree.Type (TypeF)
import qualified Core.Tree.Type as Type
import qualified Data.Kind as Kind
import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector
import Data.Void (Void)
import Semantic.Scope (Environment (..), Local)
import qualified Semantic.Scope as Scope
import Semantic.Shift (shift)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Stage (Check)
import qualified Semantic.Tree.Scheme as Solved
import qualified Semantic.Tree.TypePattern as Solved.TypePattern

type Forall = ForallOver TypeF Void

data ForallOver typef logical scope = ForallOver
  { parameters :: !(Strict.Vector (TypeF logical scope)),
    constraints :: !(ConstraintsF logical scope),
    result :: !(typef logical (Local ':+ scope))
  }

instance (Show logical, Core.Show typef) => Scope.Show (ForallOver typef logical) where
  showsPrec = showsPrec

instance (Show logical, Core.Show typef) => Show (ForallOver typef logical scope) where
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

instance (Shift.Functor (typef logical)) => Shift0.Functor (ForallOver typef logical) where
  map = Shift.mapDefault

instance (Shift.Functor (typef logical)) => Shift.Functor (ForallOver typef logical) where
  map category ForallOver {parameters, constraints, result} =
    ForallOver
      { parameters = Shift.map category <$> parameters,
        constraints = Shift.map category constraints,
        result = Shift.map (Shift.Over category) result
      }

instance
  (logical ~ Void, Substitute.TypeFunctor typef, Shift.Functor (typef Void)) =>
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

instance (Core.Functor typex) => Core.Functor (ForallOver typex) where
  map f ForallOver {parameters, constraints, result} =
    ForallOver
      { parameters = Core.map f <$> parameters,
        constraints = Core.map f constraints,
        result = Core.map f result
      }

mono :: (Shift0.Functor (typef logical)) => typef logical scope -> ForallOver typef logical scope
mono result =
  ForallOver
    { parameters = Strict.Vector.empty,
      constraints = None,
      result = shift result
    }

constraintCount :: ForallOver typef Void scope -> Constraints.ConstraintCount
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
  Kind.Type ->
  ((Kind.Type) -> Environment -> Kind.Type) ->
  ((Kind.Type) -> Environment -> Kind.Type) ->
  Kind.Type
newtype Map logical typef typef' = Map (forall scope. typef logical scope -> typef' logical scope)

map :: Map logical typef typef' -> ForallOver typef logical scope -> ForallOver typef' logical scope
map (Map map) ForallOver {parameters, constraints, result} =
  ForallOver
    { parameters,
      constraints,
      result = map result
    }
