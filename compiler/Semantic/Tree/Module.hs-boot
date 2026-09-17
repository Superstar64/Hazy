{-# LANGUAGE RoleAnnotations #-}

module Semantic.Tree.Module where

import Data.Kind (Type)
import Semantic.Layout (Layout)
import Semantic.Scope (Environment)
import Semantic.Stage (Stage)

type role Module representational nominal nominal nominal

type Module :: (Type -> Type) -> Layout -> Stage -> Environment -> Type
data Module loeb layout stage scope
