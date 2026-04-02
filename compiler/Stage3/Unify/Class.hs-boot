{-# LANGUAGE RoleAnnotations #-}

module Stage3.Unify.Class where

import Control.Monad.ST (ST)
import qualified Data.Kind as Kind
import Stage2.Scope (Environment)

type role Zonker nominal nominal

data Zonker s s'

type Zonk :: (Kind.Type -> Environment -> Kind.Type) -> Kind.Constraint
class Zonk typex where
  zonk :: Zonker s s' -> typex s scope -> ST s (typex s' scope)

type Functor :: (Environment -> Kind.Type) -> Kind.Constraint
class Functor typex
