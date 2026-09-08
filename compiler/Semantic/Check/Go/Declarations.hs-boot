{-# LANGUAGE RoleAnnotations #-}

module Semantic.Check.Go.Declarations where

import Control.Monad.ST (ST)
import Data.Functor.Identity (Identity)
import Data.Void (Void)
import Semantic.Check.Context (Context)
import Semantic.Layout (Group)
import Semantic.Scope (Environment (..))
import qualified Semantic.Scope as Scope
import Semantic.Stage (Check, Resolve)
import Semantic.Tree.Declarations (Declarations)
import qualified Semantic.Tree.Declarations as Semantic
import qualified Semantic.Unify as Unify

data Result s scope
  = !(Context s (Scope.Declaration ':+ scope))
      :&
      !(Unify.Solve s (Semantic.Local Group Check scope))

infix 5 :&

check :: Context s scope -> Semantic.Local Group Resolve scope -> ST s (Result s scope)
solve ::
  Declarations (Unify.Solve s) (Unify.Logical s scope) locality Identity Group Check scope ->
  Unify.Solve s (Declarations Identity Void locality Identity Group Check scope)
