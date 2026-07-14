{-# LANGUAGE RoleAnnotations #-}

module Semantic.Unify.Forall where

import qualified Data.Kind as Kind
import Semantic.Scope (Environment)
import Semantic.Shift (Shift)
import {-# SOURCE #-} Semantic.Unify.Type (Type)

type Forall = ForallOver Type

type role ForallOver representational nominal nominal

type ForallOver :: (Kind.Type -> Environment -> Kind.Type) -> Kind.Type -> Environment -> Kind.Type
data ForallOver typex s scope

instance (typex ~ Type) => Shift (ForallOver typex s)
