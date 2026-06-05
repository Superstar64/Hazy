{-# LANGUAGE RoleAnnotations #-}

module Semantic.Check.Temporary.Expression where

import Control.Monad.ST (ST)
import Data.Kind (Type)
import Semantic.Check.Context (Context)
import Semantic.Layout (Group)
import Semantic.Scope (Environment)
import Semantic.Stage (Check, Resolve)
import qualified Semantic.Tree.Expression as Semantic
import qualified Semantic.Tree.Expression as Solved
import qualified Semantic.Unify as Unify

type role Expression nominal nominal

type Expression :: Type -> Environment -> Type
data Expression s scope

check ::
  Context s scope ->
  Unify.Type s scope ->
  Semantic.Expression Group Resolve scope ->
  ST s (Expression s scope)
solve :: Expression s scope -> Unify.Solve s (Solved.Expression Group Check scope)
