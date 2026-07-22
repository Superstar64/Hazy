-- |
-- Unification public api
module Semantic.Unify
  ( Logical,
    Type,
    Forall,
    ForallOver,
    Constraints,
    Constraint,
    Evidence,
    Instanciation,
    forallx,
    constraints,
    none,
    constraintx,
    mono,
    variable',
    super,
    instanciation,
    monoInstanciation,
    fresh,
    mark,
    unify,
    constrain,
    Zonk (..),
    Zonker,
    Generalizable (..),
    Generalize (..),
    Body ((:::)),
    generalizeBody,
    Solve,
    liftST,
    runSolve,
    solve,
    solveEvidence,
    solveInstanciation,
    SolveForall (..),
    solveForallOver,
    solveForall,
    instanciate,
    MapForall (..),
    mapForall,
    liftWith,
    liftWith',
    lift,
    liftSchemeWith,
    liftScheme,
  )
where

import Control.Monad.ST (ST)
import Core.Substitute (Category (Substitute), logicalType, substituteType)
import qualified Core.Substitute as Substitute
import qualified Core.Tree.Constraint as Constraint (ConstraintF (..))
import qualified Core.Tree.Constraint as Simple (Constraint)
import Core.Tree.Constraints (ConstraintsF (..))
import qualified Core.Tree.Evidence as Evidence (EvidenceF (..))
import qualified Core.Tree.Evidence as Simple (Evidence)
import Core.Tree.Forall (ForallOver (ForallOver))
import qualified Core.Tree.Forall as Core (Forall)
import qualified Core.Tree.Forall as Simple (Forall, ForallOver (..))
import Core.Tree.Instanciation (InstanciationF (..))
import qualified Core.Tree.Instanciation as Simple (Instanciation)
import Core.Tree.Type (TypeF (..))
import qualified Core.Tree.Type as Simple (Type)
import qualified Data.Vector as Vector
import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector
import Semantic.Check.Context (Context (..))
import qualified Semantic.Check.Mask as Mask
import qualified Semantic.Index.Evidence as Evidence (Index (..))
import qualified Semantic.Index.Type2 as Type2
import Semantic.Scope (Environment (..), Vacuous)
import qualified Semantic.Scope as Scope
import qualified Semantic.Shift as Shift
import Semantic.Shift0 (shift)
import qualified Semantic.Shift0 as Shift0
import Semantic.Unify.Constraint (Constraint (..))
import qualified Semantic.Unify.Constraint as Constraint (solve)
import Semantic.Unify.Constraints (Constraints (..))
import Semantic.Unify.Evidence (Evidence (..))
import qualified Semantic.Unify.Evidence as Evidence (solve)
import Semantic.Unify.Forall
  ( Body ((:::)),
    Forall,
    Generalize (..),
    MapForall (..),
    SolveForall (..),
    generalizeBody,
    instanciate,
    mapForall,
  )
import qualified Semantic.Unify.Forall as Forall
import Semantic.Unify.Generalizable (Generalizable (..))
import Semantic.Unify.Instanciation (Instanciation (..))
import qualified Semantic.Unify.Instanciation as Instanciation
import Semantic.Unify.Solve (Solve (..))
import Semantic.Unify.Type (Logical, Type (..))
import qualified Semantic.Unify.Type as Type (constrain, fresh, mark, solve, unify)
import Semantic.Unify.Zonk (Zonk (..), Zonker)
import Syntax.Position (Position)
import Prelude hiding (Functor, head)

-- todo, this function isn't safe
forallx ::
  Strict.Vector (Type s scope) ->
  Constraints s scope ->
  typef (Logical s) (Scope.Local ':+ scope) ->
  ForallOver typef (Logical s) scope
forallx parameters constraints result =
  ForallOver
    { parameters,
      constraints,
      result
    }

constraints :: Strict.Vector (Constraint s scope) -> Constraints s scope
constraints constraints = Constraints constraints

none :: Constraints s scope
none = None

constraintx ::
  Type2.Index scope ->
  Int ->
  Strict.Vector (Type s (Scope.Local ':+ scope)) ->
  Constraint s scope
constraintx classx head arguments =
  Constraint.Constraint
    { classx,
      head,
      arguments
    }

mono :: (Shift0.Functor (typex s)) => typex s scope -> ForallOver typex s scope
mono result =
  ForallOver
    { parameters = Strict.Vector.empty,
      constraints = None,
      result = shift result
    }

variable' :: Evidence.Index scope -> Instanciation s scope -> Evidence s scope
variable' variable instanciation = Evidence.Variable {variable, instanciation}

super :: Evidence s scope -> Int -> Evidence s scope
super base index =
  Evidence.Super
    { base,
      index
    }

instanciation :: Strict.Vector (Evidence s scope) -> Instanciation s scope
instanciation instanciation = Instanciation instanciation

monoInstanciation :: Instanciation s scope
monoInstanciation = Mono

-- todo, figure out how to make sure nodes that are being solved early have a
-- rigid context
runSolve :: Solve s a -> ST s a
runSolve (Solve a) = a

-- todo, figure out how to make sure no unification occurs during solving
liftST :: ST s a -> Solve s a
liftST = Solve

solveEvidence :: Position -> Evidence s scope -> Solve s (Simple.Evidence scope)
solveEvidence position evidence = Evidence.solve position evidence

solveInstanciation :: Position -> Instanciation s scope -> Solve s (Simple.Instanciation scope)
solveInstanciation position instanciation = Instanciation.solve position instanciation

solveConstraint :: Position -> Constraint s scope -> Solve s (Simple.Constraint scope)
solveConstraint position constraint = Constraint.solve position constraint

solveForall :: Position -> Forall s scope -> Solve s (Simple.Forall scope)
solveForall = solveForallOver (SolveForall solve)

solveForallOver ::
  SolveForall typef typef' ->
  Position ->
  ForallOver typef (Logical s) scope ->
  Solve s (Simple.ForallOver typef' Vacuous scope)
solveForallOver solve position scheme = Forall.solve solve position scheme

fresh :: Type s scope -> ST s (Type s scope)
fresh typex = Type.fresh typex

mark :: Context s scope -> Position -> Mask.Erasure -> Type s scope -> ST s ()
mark context position erasure typex = Type.mark context position erasure typex

solve :: Position -> Type s scope -> Solve s (Simple.Type scope)
solve position typex = Type.solve position typex

-- | Unify two types
--
-- The first argument is the expected type.
-- The second argument is the actual type.
--
-- Both arguments must be well kinded, though they may have different kinds.
-- Alternatively, they may be untypeable, in which case they never unify with
-- unification variables and instead only do syntatic equality.
unify :: Context s scope -> Position -> Type s scope -> Type s scope -> ST s ()
unify context position type1 type2 = Type.unify context position type1 type2

constrain ::
  Context s scope ->
  Position ->
  Type2.Index scope ->
  Type s scope ->
  ST s (Evidence s scope)
constrain context position classx argument = do
  evidence <- Type.constrain context position classx argument
  pure evidence

liftWith :: Strict.Vector (Type s scope) -> TypeF Vacuous (Scope.Local ':+ scope) -> Type s scope
liftWith substitution typex = substituteType (Strict.Vector.toLazy $ substitution) typex

liftWith' ::
  Strict.Vector (Type s scope) ->
  TypeF Vacuous (Scope.Local ':+ Scope.Local ':+ scope) ->
  Type s (Scope.Local ':+ scope)
liftWith' substitution typex = Substitute.mapType substitute typex
  where
    substitute = Substitute.Over $ Substitute Shift.Id wrapped Vector.empty
    wrapped = Strict.Vector.toLazy substitution

lift :: TypeF Vacuous scope -> Type s scope
lift = logicalType

liftSchemeWith :: Strict.Vector (Type s scope) -> Core.Forall (Scope.Local ':+ scope) -> Forall s scope
liftSchemeWith substitution forallx =
  case substituteType (Strict.Vector.toLazy substitution) forallx of
    ForallOver {parameters, constraints, result} ->
      ForallOver
        { parameters,
          constraints,
          result
        }

liftScheme :: Core.Forall scope -> Forall s scope
liftScheme = liftSchemeWith Strict.Vector.empty . shift
