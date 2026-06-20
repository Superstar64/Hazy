{-# LANGUAGE RoleAnnotations #-}

module Semantic.Unify.Constraints where

import Data.Kind (Type)
import Semantic.Scope (Environment)

type role Constraints nominal nominal

type Constraints :: Type -> Environment -> Type
data Constraints s scope
