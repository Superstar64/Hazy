{-# LANGUAGE RoleAnnotations #-}

module Semantic.Unify.SchemeOver where

import qualified Data.Kind as Kind
import Semantic.Scope (Environment)

type role SchemeOver representational nominal nominal

type SchemeOver :: (Kind.Type -> Environment -> Kind.Type) -> Kind.Type -> Environment -> Kind.Type
data SchemeOver typex s scope
