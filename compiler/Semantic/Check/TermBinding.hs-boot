{-# LANGUAGE RoleAnnotations #-}

module Semantic.Check.TermBinding where

import Data.Kind (Type)
import Semantic.Scope (Environment)

type role TermBinding nominal nominal

type TermBinding :: Type -> Environment -> Type
data TermBinding s scope
