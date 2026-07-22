{-# LANGUAGE RoleAnnotations #-}

module Semantic.Unify
  ( module Semantic.Unify,
    Evidence,
    Zonk (..),
    Generalizable (..),
    Constraints,
    Constraint,
    Instanciation,
    Forall,
    ForallOver,
    Type,
    Solve,
    instanciate,
  )
where

import Control.Monad.ST (ST)
import Core.Tree.Forall (ForallOver)
import qualified Core.Tree.Forall as Core (Forall)
import Core.Tree.Type (TypeF)
import qualified Core.Tree.Type as Simple
import qualified Data.Vector.Strict as Strict
import {-# SOURCE #-} Semantic.Check.Context (Context)
import qualified Semantic.Check.Mask as Mask
import qualified Semantic.Index.Evidence as Evidence
import qualified Semantic.Index.Type2 as Type2
import Semantic.Scope (Environment ((:+)), Vacuous)
import qualified Semantic.Scope as Scope
import qualified Semantic.Shift0 as Shift0
import {-# SOURCE #-} Semantic.Unify.Constraint hiding (solve, unify)
import {-# SOURCE #-} Semantic.Unify.Constraints (Constraints)
import Semantic.Unify.Evidence (Evidence)
import {-# SOURCE #-} Semantic.Unify.Forall (Forall, instanciate)
import {-# SOURCE #-} Semantic.Unify.Generalizable (Generalizable (..))
import {-# SOURCE #-} Semantic.Unify.Instanciation hiding (solve, unify)
import Semantic.Unify.Solve (Solve)
import {-# SOURCE #-} Semantic.Unify.Type
import {-# SOURCE #-} Semantic.Unify.Zonk (Zonk (..))
import Syntax.Position (Position)

mono :: (Shift0.Functor (typef logical)) => typef logical scope -> ForallOver typef logical scope
variable' :: Evidence.Index scope -> Instanciation s scope -> Evidence s scope
super :: Evidence s scope -> Int -> Evidence s scope
instanciation :: Strict.Vector (Evidence s scope) -> Instanciation s scope
monoInstanciation :: Instanciation s scope
forallx ::
  Strict.Vector (Type s scope) ->
  Constraints s scope ->
  typef (Logical s) (Scope.Local ':+ scope) ->
  ForallOver typef (Logical s) scope
constraints :: Strict.Vector (Constraint s scope) -> Constraints s scope
none :: Constraints s scope
constraintx ::
  Type2.Index scope ->
  Int ->
  Strict.Vector (Type s (Scope.Local ':+ scope)) ->
  Constraint s scope
liftST :: ST s a -> Solve s a
runSolve :: Solve s a -> ST s a
fresh :: Type s scope -> ST s (Type s scope)
mark :: Context s scope -> Position -> Mask.Erasure -> Type s scope -> ST s ()
solve :: Position -> Type s scope -> Solve s (Simple.Type scope)
unify :: Context s scope -> Position -> Type s scope -> Type s scope -> ST s ()
liftWith :: Strict.Vector (Type s scope) -> TypeF Vacuous (Scope.Local ':+ scope) -> Type s scope
liftWith' ::
  Strict.Vector (Type s scope) ->
  TypeF Vacuous (Scope.Local ':+ Scope.Local ':+ scope) ->
  Type s (Scope.Local ':+ scope)
lift :: TypeF Vacuous scope -> Type s scope
liftScheme :: Core.Forall scope -> Forall s scope
