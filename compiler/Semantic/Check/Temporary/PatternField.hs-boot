{-# LANGUAGE RoleAnnotations #-}

module Semantic.Check.Temporary.PatternField where

import Data.Kind (Type)
import Semantic.Scope (Environment)

type role Field nominal nominal

type Field :: Type -> Environment -> Type
data Field s scope
