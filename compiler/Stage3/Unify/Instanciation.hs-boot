{-# LANGUAGE RoleAnnotations #-}

module Stage3.Unify.Instanciation where

import qualified Data.Kind as Kind
import Stage2.Scope (Environment)

type role Instanciation nominal nominal

type Instanciation :: Kind.Type -> Environment -> Kind.Type
data Instanciation s scope
