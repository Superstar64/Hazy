{-# LANGUAGE RoleAnnotations #-}

module Semantic.Unify
  ( module Semantic.Unify,
    Evidence,
    Zonk (..),
    Constraints,
    Constraint,
    Instanciation,
    Forall,
    ForallOver,
    Type,
    Solve,
  )
where

import Control.Monad.ST (ST)
import qualified Core.Tree.Forall as Core (Forall)
import Core.Tree.Type (TypeF)
import qualified Core.Tree.Type as Simple
import qualified Data.Vector.Strict as Strict
import {-# SOURCE #-} Semantic.Check.Context (Context)
import qualified Semantic.Check.Mask as Mask
import qualified Semantic.Index.Constructor as Constructor
import qualified Semantic.Index.Evidence as Evidence
import qualified Semantic.Index.Local as Local
import qualified Semantic.Index.Type as Type
import qualified Semantic.Index.Type2 as Type2
import Semantic.Scope (Environment ((:+)), Vacuous)
import qualified Semantic.Scope as Scope
import Semantic.Shift (Shift)
import {-# SOURCE #-} Semantic.Unify.Class
import {-# SOURCE #-} Semantic.Unify.Constraint hiding (solve, unify)
import {-# SOURCE #-} Semantic.Unify.Constraints (Constraints)
import Semantic.Unify.Evidence (Evidence)
import {-# SOURCE #-} Semantic.Unify.Forall (Forall, ForallOver)
import {-# SOURCE #-} Semantic.Unify.Instanciation hiding (solve, unify)
import {-# SOURCE #-} Semantic.Unify.Type
import Syntax.Position (Position)

mono :: (Shift (typex s)) => typex s scope -> ForallOver typex s scope
variable :: Local.Index scope -> Type s scope
constructor :: Type2.Index scope -> Type s scope
call :: Type s scope -> Type s scope -> Type s scope
index :: Type.Index scope -> Type s scope
lifted :: Constructor.Index scope -> Type s scope
arrow :: Type s scope
list :: Type s scope
listWith :: Type s scope -> Type s scope
tuple :: Int -> Type s scope
typex :: Type s scope
kind :: Type s scope
typeWith :: Type s scope -> Type s scope

infixr 0 `function`

function :: Type s scope -> Type s scope -> Type s scope
constraint :: Type s scope
levity :: Type s scope
small :: Type s scope
large :: Type s scope
universe :: Type s scope
variable' :: Evidence.Index scope -> Instanciation s scope -> Evidence s scope
super :: Evidence s scope -> Int -> Evidence s scope
instanciation :: Strict.Vector (Evidence s scope) -> Instanciation s scope
monoInstanciation :: Instanciation s scope
forallx ::
  Strict.Vector (Type s scope) ->
  Constraints s scope ->
  typex s (Scope.Local ':+ scope) ->
  ForallOver typex s scope
constraints :: Strict.Vector (Constraint s scope) -> Constraints s scope
none :: Constraints s scope
constraintx ::
  Type2.Index scope ->
  Int ->
  Strict.Vector (Type s (Scope.Local ':+ scope)) ->
  Constraint s scope
instanciate :: Context s scope -> Position -> Forall s scope -> ST s (Type s scope, Instanciation s scope)
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
