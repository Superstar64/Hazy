{-# LANGUAGE RoleAnnotations #-}

module Stage3.Temporary.Type where

import Control.Monad.ST (ST)
import qualified Data.Kind as Kind (Type)
import Stage1.Position (Position)
import Stage2.Scope (Environment)
import qualified Stage2.Tree.Type as Stage2
import {-# SOURCE #-} Stage3.Check.Context (Context)
import qualified Stage3.Synonym as Synonym
import {-# SOURCE #-} qualified Stage3.Tree.Type as Solved
import {-# SOURCE #-} qualified Stage3.Unify as Unify

type role Type phantom nominal

type Type :: Kind.Type -> Environment -> Kind.Type
data Type s scope

check :: Context s scope -> Unify.Type s scope -> Stage2.Type Position scope -> ST s (Type s scope)
solve :: Synonym.Context s scope -> Type s scope -> ST s (Solved.Type scope)
