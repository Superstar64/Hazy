{-# LANGUAGE RoleAnnotations #-}

module Stage3.Temporary.Pattern where

import Control.Monad.ST (ST)
import Data.Kind (Type)
import qualified Stage2.Index.Table.Term as Term
import Stage2.Scope (Environment ((:+)))
import qualified Stage2.Tree.Pattern as Stage2
import Stage3.Check.Context (Context)
import Stage3.Check.TermBinding (TermBinding)
import qualified Stage3.Tree.Pattern as Solved
import {-# SOURCE #-} qualified Stage3.Unify as Unify

type role Pattern nominal nominal

type Pattern :: Type -> Environment -> Type
data Pattern s scope

augmentPattern :: Pattern s scopes -> Term.Bound (TermBinding s) (scope ':+ scopes)
check :: Context s scope -> Unify.Type s scope -> Stage2.Pattern scope -> ST s (Pattern s scope)
solve :: Pattern s scope -> ST s (Solved.Pattern scope)
