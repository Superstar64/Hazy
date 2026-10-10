{-# LANGUAGE RoleAnnotations #-}

module Semantic.Check.Binding.Type where

import Data.Kind (Type)
import Semantic.Scope (Environment)

type role TypeBinding nominal nominal

type TypeBinding :: Type -> Environment -> Type
data TypeBinding s scope
