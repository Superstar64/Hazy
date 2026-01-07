{-# LANGUAGE RoleAnnotations #-}

module Stage3.Temporary.Definition where

import Control.Monad.ST (ST)
import Data.Kind (Type)
import Stage2.Scope (Environment)
import qualified Stage2.Tree.Definition as Stage2
import Stage3.Check.Context (Context)
import qualified Stage3.Tree.Definition as Solved
import {-# SOURCE #-} qualified Stage3.Unify as Unify

type role Definition nominal nominal

type Definition :: Type -> Environment -> Type
data Definition s scope

check :: Context s scope -> Unify.Type s scope -> Stage2.Definition scope -> ST s (Definition s scope)
solve :: Definition s scope -> ST s (Solved.Definition scope)
