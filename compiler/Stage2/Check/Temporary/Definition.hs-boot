{-# LANGUAGE RoleAnnotations #-}

module Stage2.Check.Temporary.Definition where

import Control.Monad.ST (ST)
import Data.Kind (Type)
import Stage2.Layout (Group)
import Stage2.Scope (Environment)
import Stage2.Stage (Check, Resolve)
import qualified Stage2.Tree.Definition as Solved
import qualified Stage2.Tree.Definition as Stage2
import Stage2.Check.Context (Context)
import {-# SOURCE #-} qualified Stage2.Unify as Unify

type role Definition nominal nominal

type Definition :: Type -> Environment -> Type
data Definition s scope

check ::
  Context s scope ->
  Unify.Type s scope ->
  Stage2.Definition Group Resolve scope ->
  ST s (Definition s scope)
solve :: Definition s scope -> Unify.Solve s (Solved.Definition Group Check scope)
