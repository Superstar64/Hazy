{-# LANGUAGE RoleAnnotations #-}

module Semantic.Unify.Instanciation where

import qualified Data.Kind as Kind
import Semantic.Scope (Environment)

type role Instanciation nominal nominal

type Instanciation :: Kind.Type -> Environment -> Kind.Type
data Instanciation s scope
