{-# LANGUAGE RoleAnnotations #-}

module Semantic.Tree.Instance where

import Data.Kind (Type)
import Semantic.Layout (Layout)
import Semantic.Scope (Environment)
import Semantic.Stage (Stage)

type role Instance nominal representational nominal nominal nominal

type Instance :: (Type -> Type) -> (Type -> Type) -> Layout -> Stage -> Environment -> Type
data Instance solve loeb layout stage scope
