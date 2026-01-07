{-# LANGUAGE RoleAnnotations #-}

module Stage3.Temporary.Expression where

import Control.Monad.ST (ST)
import Data.Kind (Type)
import Stage2.Scope (Environment)
import qualified Stage2.Tree.Expression as Stage2
import Stage3.Check.Context (Context)
import qualified Stage3.Tree.Expression as Solved
import qualified Stage3.Unify as Unify

type role Expression nominal nominal

type Expression :: Type -> Environment -> Type
data Expression s scope

check :: Context s scope -> Unify.Type s scope -> Stage2.Expression scope -> ST s (Expression s scope)
solve :: Expression s scope -> ST s (Solved.Expression scope)
