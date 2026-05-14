{-# LANGUAGE RoleAnnotations #-}

module Stage3.Unify.Class where

import Control.Monad.ST (ST)
import qualified Data.Kind as Kind
import Stage2.Scope (Environment)
import Stage2.Shift (Shift)
import qualified Stage2.Shift as Shift

type role Zonker nominal nominal

data Zonker s s'

type Zonk :: (Kind.Type -> Environment -> Kind.Type) -> Kind.Constraint
class Zonk typex where
  zonk :: Zonker s s' -> typex s scope -> ST s (typex s' scope)

type Delay :: (Environment -> Kind.Type) -> Kind.Type -> Environment -> Kind.Type
newtype Delay f s scope = Delay {solveDelay :: ST s (f scope)}

instance (Shift f) => Shift (Delay f s)

instance (Shift.Functor f) => Shift.Functor (Delay f s)

type Functor :: (Environment -> Kind.Type) -> Kind.Constraint
class Functor typex
