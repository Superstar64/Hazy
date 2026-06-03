{-# LANGUAGE RoleAnnotations #-}

module Stage3.Temporary.Pattern where

import Control.Monad.ST (ST)
import Data.Kind (Type)
import qualified Stage2.Index.Table.Term as Term
import Stage2.Index.Term (Bound)
import Stage2.Scope (Environment ((:+)))
import Stage2.Stage (Check, Resolve)
import qualified Stage2.Tree.Pattern as Solved
import qualified Stage2.Tree.Pattern as Stage2
import Stage3.Check.Context (Context)
import Stage3.Check.TermBinding (TermBinding)
import {-# SOURCE #-} qualified Stage3.Unify as Unify

type role Pattern nominal nominal

type Pattern :: Type -> Environment -> Type
data Pattern s scope

(!) :: Pattern s scope -> Bound -> Unify.Type s scope
augmentPattern :: Pattern s scopes -> Term.Bound (TermBinding s) (scope ':+ scopes)
check :: Context s scope -> Unify.Type s scope -> Stage2.Pattern Resolve scope -> ST s (Pattern s scope)
solve :: Pattern s scope -> ST s (Solved.Pattern Check scope)
