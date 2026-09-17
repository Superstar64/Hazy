{-# LANGUAGE RoleAnnotations #-}

module Semantic.Check.Go.Declarations where

import Control.Monad.ST (ST)
import Semantic.Check.Context (Context)
import Semantic.Layout (Group)
import Semantic.Scope (Environment (..))
import qualified Semantic.Scope as Scope
import Semantic.Stage (Check, Resolve)
import qualified Semantic.Tree.Declarations as Semantic
import qualified Semantic.Unify as Unify

data Result s scope
  = !(Context s (Scope.Declaration ':+ scope))
      :&
      !(Unify.Solve s (Semantic.Local Group Check scope))

infix 5 :&

check :: Context s scope -> Semantic.Local Group Resolve scope -> ST s (Result s scope)
