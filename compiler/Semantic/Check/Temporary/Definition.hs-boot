{-# LANGUAGE RoleAnnotations #-}

module Semantic.Check.Temporary.Definition where

import Control.Monad.ST (ST)
import Data.Kind (Type)
import Semantic.Check.Context (Context)
import Semantic.Layout (Group)
import Semantic.Scope (Environment)
import Semantic.Stage (Check, Resolve)
import qualified Semantic.Tree.Definition as Semantic
import qualified Semantic.Tree.Definition as Solved
import {-# SOURCE #-} qualified Semantic.Unify as Unify

type role Definition nominal nominal

type Definition :: Type -> Environment -> Type
data Definition s scope

check ::
  Context s scope ->
  Unify.Type s scope ->
  Semantic.Definition Group Resolve scope ->
  ST s (Definition s scope)
solve :: Definition s scope -> Unify.Solve s (Solved.Definition Group Check scope)
