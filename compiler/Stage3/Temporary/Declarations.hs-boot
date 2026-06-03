{-# LANGUAGE RoleAnnotations #-}

module Stage3.Temporary.Declarations where

import Control.Monad.ST (ST)
import Data.Kind (Type)
import Stage2.Layout (Group)
import Stage2.Locality (Locality)
import qualified Stage2.Locality as Locality
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import Stage2.Stage (Check, Resolve)
import qualified Stage2.Tree.Declarations as Stage2 (Declarations)
import Stage3.Check.Context (Context)
import qualified Stage3.Tree.Declarations as Solved

type role Declarations nominal nominal nominal

type Declarations :: Locality -> Type -> Environment -> Type
data Declarations locality s scope

check ::
  Context s scope ->
  Stage2.Declarations Locality.Local Group Resolve (Scope.Declaration ':+ scope) ->
  ST
    s
    ( Context s (Scope.Declaration ':+ scope),
      Declarations Locality.Local s (Scope.Declaration ':+ scope)
    )
solve :: Declarations locality s scope -> ST s (Solved.Declarations locality Group Check scope)
