{-# LANGUAGE RoleAnnotations #-}

module Semantic.Unify.Constraint where

import qualified Data.Kind as Kind
import Semantic.Scope (Environment)

type role Constraint nominal nominal

type Constraint :: Kind.Type -> Environment -> Kind.Type
data Constraint s scope
