{-# LANGUAGE RoleAnnotations #-}

module Stage3.Temporary.Type where

import Control.Monad.ST (ST)
import qualified Data.Kind as Kind (Type)
import Stage1.Position (Position)
import Stage2.Scope (Environment)
import Stage2.Stage (Check, Resolve)
import qualified Stage2.Tree.Type as Stage2
import {-# SOURCE #-} qualified Stage2.Tree.Type as Solved
import {-# SOURCE #-} Stage3.Check.Context (Context)
import {-# SOURCE #-} qualified Stage3.Unify as Unify

type role Type phantom nominal

type Type :: Kind.Type -> Environment -> Kind.Type
data Type s scope

check :: Context s scope -> Unify.Type s scope -> Stage2.Type Position Resolve scope -> ST s (Type s scope)
solve :: Context s scope -> Type s scope -> ST s (Solved.Type Position Check scope)
