{-# LANGUAGE RoleAnnotations #-}

module Semantic.Check.Temporary.Statements where

import Control.Monad.ST (ST)
import Data.Kind (Type)
import Semantic.Check.Context (Context)
import Semantic.Layout (Group)
import Semantic.Scope (Environment)
import Semantic.Stage (Check, Resolve)
import qualified Semantic.Tree.Statements as Semantic
import qualified Semantic.Tree.Statements as Solved
import qualified Semantic.Unify as Unify

type role Statements nominal nominal

type Statements :: Type -> Environment -> Type
data Statements s scope

check ::
  Context s scope ->
  Unify.Type s scope ->
  Semantic.Statements Semantic.Guard Group Resolve scope ->
  ST s (Statements s scope)
solve :: Statements s scope -> Unify.Solve s (Solved.Statements Solved.Guard Group Check scope)
