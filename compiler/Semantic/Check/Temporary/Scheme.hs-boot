{-# LANGUAGE RoleAnnotations #-}

module Semantic.Check.Temporary.Scheme where

import Control.Monad.ST (ST)
import Data.Kind (Type)
import {-# SOURCE #-} Semantic.Check.Context (Context)
import Semantic.Scope (Environment)
import Semantic.Stage (Check, Resolve)
import qualified Semantic.Tree.Scheme as Semantic
import {-# SOURCE #-} qualified Semantic.Tree.Scheme as Solved
import {-# SOURCE #-} qualified Semantic.Unify as Unify
import Syntax.Position (Position)

type role Scheme nominal nominal

type Scheme :: Type -> Environment -> Type
data Scheme s scope

check :: Context s scope -> Semantic.Scheme Position Resolve scope -> ST s (Scheme s scope)
solve :: Context s scope -> Scheme s scope -> Unify.Solve s (Solved.Scheme Position Check scope)
