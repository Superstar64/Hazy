{-# LANGUAGE RoleAnnotations #-}

module Semantic.Check.Temporary.Type where

import Control.Monad.ST (ST)
import qualified Data.Kind as Kind (Type)
import {-# SOURCE #-} Semantic.Check.Context (Context)
import Semantic.Scope (Environment)
import Semantic.Stage (Check, Resolve)
import qualified Semantic.Tree.Type as Semantic
import {-# SOURCE #-} qualified Semantic.Tree.Type as Solved
import {-# SOURCE #-} qualified Semantic.Unify as Unify
import Syntax.Position (Position)

type role Type phantom nominal

type Type :: Kind.Type -> Environment -> Kind.Type
data Type s scope

check :: Context s scope -> Unify.Type s scope -> Semantic.Type Position Resolve scope -> ST s (Type s scope)
solve :: Context s scope -> Type s scope -> Unify.Solve s (Solved.Type Position Check scope)
