{-# LANGUAGE RoleAnnotations #-}

module Stage2.Check.Temporary.Expression where

import Control.Monad.ST (ST)
import Data.Kind (Type)
import Stage2.Layout (Group)
import Stage2.Scope (Environment)
import Stage2.Stage (Check, Resolve)
import qualified Stage2.Tree.Expression as Solved
import qualified Stage2.Tree.Expression as Stage2
import Stage2.Check.Context (Context)
import qualified Stage2.Unify as Unify

type role Expression nominal nominal

type Expression :: Type -> Environment -> Type
data Expression s scope

check ::
  Context s scope ->
  Unify.Type s scope ->
  Stage2.Expression Group Resolve scope ->
  ST s (Expression s scope)
solve :: Expression s scope -> Unify.Solve s (Solved.Expression Group Check scope)
