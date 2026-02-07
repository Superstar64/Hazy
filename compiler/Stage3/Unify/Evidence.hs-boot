{-# LANGUAGE RoleAnnotations #-}

module Stage3.Unify.Evidence where

import qualified Data.Kind as Kind
import Stage2.Scope (Environment)

type role Evidence nominal nominal

type Evidence :: Kind.Type -> Environment -> Kind.Type
data Evidence s scope
