{-# LANGUAGE RoleAnnotations #-}

module Semantic.Tree.Pattern where

import Data.Kind (Type)
import Semantic.Scope (Environment)
import Semantic.Stage (Stage)

type role Pattern representational nominal nominal

type Pattern :: Type -> Stage -> Environment -> Type
data Pattern position stage scope
