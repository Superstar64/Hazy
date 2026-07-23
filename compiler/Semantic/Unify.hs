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
  )
where

import Control.Monad.ST (ST)
import qualified Core.Tree.Constraint as Simple (Constraint)
import qualified Core.Tree.Evidence as Simple (Evidence)
import Core.Tree.Forall (ForallOver)
import qualified Core.Tree.Forall as Simple (Forall, ForallOver (..))
import qualified Core.Tree.Instanciation as Simple (Instanciation)
import qualified Core.Tree.Type as Simple (Type)
import Semantic.Check.Context (Context (..))
import qualified Semantic.Check.Mask as Mask
import qualified Semantic.Index.Type2 as Type2
import Semantic.Scope (Vacuous)
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
