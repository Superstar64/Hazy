{-# LANGUAGE RoleAnnotations #-}

module Stage3.Temporary.Statements where

import Control.Monad.ST (ST)
import Data.Kind (Type)
import Stage2.Layout (Group)
import Stage2.Scope (Environment)
import Stage2.Stage (Check, Resolve)
import qualified Stage2.Tree.Statements as Solved
import qualified Stage2.Tree.Statements as Stage2
import Stage3.Check.Context (Context)
import qualified Stage3.Unify as Unify

type role Statements nominal nominal

type Statements :: Type -> Environment -> Type
data Statements s scope

instance Unify.Zonk Statements

check ::
  Context s scope ->
  Unify.Type s scope ->
  Stage2.Statements Stage2.Guard Group Resolve scope ->
  ST s (Statements s scope)
solve :: Statements s scope -> ST s (Solved.Statements Solved.Guard Group Check scope)
