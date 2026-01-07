{-# LANGUAGE RoleAnnotations #-}

module Stage3.Temporary.Declarations where

import Control.Monad.ST (ST)
import Data.Kind (Type)
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import qualified Stage2.Tree.Declarations as Stage2 (Declarations)
import Stage3.Check.Context (Context)
import qualified Stage3.Tree.Declarations as Solved

type role Declarations nominal nominal

type Declarations :: Type -> Environment -> Type
data Declarations s scope

check ::
  Context s scope ->
  Stage2.Declarations (Scope.Declaration ':+ scope) ->
  ST
    s
    ( Context s (Scope.Declaration ':+ scope),
      Declarations s (Scope.Declaration ':+ scope)
    )
solve :: Declarations s scope -> ST s (Solved.Declarations scope)
