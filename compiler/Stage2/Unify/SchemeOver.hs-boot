{-# LANGUAGE RoleAnnotations #-}

module Stage2.Unify.SchemeOver where

import qualified Data.Kind as Kind
import Stage2.Scope (Environment)

type role SchemeOver representational nominal nominal

type SchemeOver :: (Kind.Type -> Environment -> Kind.Type) -> Kind.Type -> Environment -> Kind.Type
data SchemeOver typex s scope
