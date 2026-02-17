{-# LANGUAGE RoleAnnotations #-}

module Stage3.Unify.Constraint where

import qualified Data.Kind as Kind
import Stage2.Scope (Environment)

type role Constraint nominal nominal

type Constraint :: Kind.Type -> Environment -> Kind.Type
data Constraint s scope
