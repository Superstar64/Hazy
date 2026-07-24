{-# LANGUAGE RoleAnnotations #-}

module Semantic.Unify.Forall where

import Control.Monad.ST (ST)
import Core.Tree.Forall (ForallOver)
import Core.Tree.Type (TypeF)
import qualified Core.Tree.TypeLambda as Simple
import qualified Data.Kind as Kind
import {-# SOURCE #-} Semantic.Check.Context (Context)
import Semantic.Scope (Environment (..))
import {-# SOURCE #-} Semantic.Unify.Generalizable (Generalizable)
import {-# SOURCE #-} Semantic.Unify.Instanciation
import {-# SOURCE #-} Semantic.Unify.Solve (Solve)
import {-# SOURCE #-} Semantic.Unify.Type
import Syntax.Position (Position)

type Forall s = ForallOver TypeF (Logical s)

newtype Generalize typex s scopes = Generalize
  { runGeneralize ::
      forall scope.
      Context s (scope ':+ scopes) ->
      ST s (typex s (scope ':+ scopes))
  }

type Body ::
  ((Environment -> Kind.Type) -> Environment -> Kind.Type) ->
  (Environment -> Kind.Type) ->
  Kind.Type ->
  Environment ->
  Kind.Type
data Body typef term s scope = (:::)
  { typex :: !(typef (Logical s) scope),
    term :: !(Solve s (term scope))
  }

infix 5 :::

instanciate :: Context s scope -> Position -> Forall s scope -> ST s (Type s scope, Instanciation s scope)
generalizeBody ::
  (Generalizable typex) =>
  Position ->
  Context s scope ->
  Generalize (Body typex term) s scope ->
  ST s (Body (ForallOver typex) (Simple.TypeLambdaOver term) s scope)

type MapForall ::
  ((Environment -> Kind.Type) -> Environment -> Kind.Type) ->
  ((Environment -> Kind.Type) -> Environment -> Kind.Type) ->
  Kind.Type
newtype MapForall typef typef' = MapForall (forall s scope. typef (Logical s) scope -> typef' (Logical s) scope)

mapForall :: MapForall typef typef' -> ForallOver typef (Logical s) scope -> ForallOver typef' (Logical s) scope
