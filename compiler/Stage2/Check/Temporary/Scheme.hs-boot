{-# LANGUAGE RoleAnnotations #-}

module Stage2.Check.Temporary.Scheme where

import Control.Monad.ST (ST)
import Data.Kind (Type)
import Stage1.Position (Position)
import Stage2.Scope (Environment)
import Stage2.Stage (Check, Resolve)
import qualified Stage2.Tree.Scheme as Stage2
import {-# SOURCE #-} qualified Stage2.Tree.Scheme as Solved
import {-# SOURCE #-} Stage2.Check.Context (Context)
import {-# SOURCE #-} qualified Stage2.Unify as Unify

type role Scheme nominal nominal

type Scheme :: Type -> Environment -> Type
data Scheme s scope

check :: Context s scope -> Stage2.Scheme Position Resolve scope -> ST s (Scheme s scope)
solve :: Context s scope -> Scheme s scope -> Unify.Solve s (Solved.Scheme Position Check scope)
