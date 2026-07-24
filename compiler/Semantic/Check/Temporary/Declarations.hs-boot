{-# LANGUAGE RoleAnnotations #-}

module Semantic.Check.Temporary.Declarations where

import Control.Monad.ST (ST)
import Data.Kind (Type)
import Semantic.Check.Context (Context)
import Semantic.Layout (Group)
import Semantic.Locality (Locality)
import qualified Semantic.Locality as Locality
import Semantic.Scope (Environment (..))
import qualified Semantic.Scope as Scope
import Semantic.Stage (Check, Resolve)
import qualified Semantic.Tree.Declarations as Semantic
import qualified Semantic.Tree.Declarations as Solved
import qualified Semantic.Unify as Unify

type role Declarations nominal nominal nominal

type Declarations :: Locality -> Type -> Environment -> Type
data Declarations locality s scope

newtype Local s scope = Local (Declarations Locality.Local s (Scope.Declaration ':+ scope))

check ::
  Context s scope ->
  Semantic.Local Group Resolve scope ->
  ST
    s
    ( Context s (Scope.Declaration ':+ scope),
      Local s scope
    )
solve :: Declarations locality s scope -> Unify.Solve s (Solved.Declarations locality Group Check scope)
solveLocal :: Local s scope -> Unify.Solve s (Solved.Local Group Check scope)
