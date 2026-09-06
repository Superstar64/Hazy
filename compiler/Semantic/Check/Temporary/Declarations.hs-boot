{-# LANGUAGE RoleAnnotations #-}

module Semantic.Check.Temporary.Declarations where

import Control.Monad.ST (ST)
import Data.Functor.Identity (Identity)
import Data.Kind (Type)
import Data.Void (Void)
import Semantic.Check.Context (Context)
import Semantic.Layout (Group)
import Semantic.Locality (Locality)
import Semantic.Scope (Environment (..))
import qualified Semantic.Scope as Scope
import Semantic.Stage (Check, Resolve)
import qualified Semantic.Tree.Declarations as Semantic
import qualified Semantic.Tree.Declarations as Solved
import qualified Semantic.Unify as Unify

type role Declarations nominal nominal nominal

type Declarations :: Locality -> Type -> Environment -> Type
data Declarations locality s scope

data Result s scope
  = !(Context s (Scope.Declaration ':+ scope))
      :&
      !(Unify.Solve s (Semantic.Local Group Check scope))

infix 5 :&

check :: Context s scope -> Semantic.Local Group Resolve scope -> ST s (Result s scope)
solve ::
  Declarations locality s scope ->
  Unify.Solve s (Solved.Declarations Identity Void locality Identity Group Check scope)
