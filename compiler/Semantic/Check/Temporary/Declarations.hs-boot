{-# LANGUAGE RoleAnnotations #-}

module Semantic.Check.Temporary.Declarations where

import Control.Monad.ST (ST)
import Data.Kind (Type)
import Semantic.Check.Context (Context)
import qualified Semantic.Check.Go.Declarations as Solved
import Semantic.Layout (Group)
import Semantic.Locality (Locality)
import qualified Semantic.Locality as Locality
import Semantic.Scope (Environment (..))
import qualified Semantic.Scope as Scope
import Semantic.Stage (Check, Resolve)
import qualified Semantic.Tree.Declarations as Semantic (Declarations)
import {-# SOURCE #-} qualified Semantic.Unify as Unify

type role Declarations nominal nominal nominal

type Declarations :: Locality -> Type -> Environment -> Type
data Declarations locality s scope

check ::
  Context s scope ->
  Semantic.Declarations Locality.Local Group Resolve (Scope.Declaration ':+ scope) ->
  ST
    s
    ( Context s (Scope.Declaration ':+ scope),
      Declarations Locality.Local s (Scope.Declaration ':+ scope)
    )
solve :: Declarations locality s scope -> Unify.Solve s (Solved.Declarations locality Group Check scope)
