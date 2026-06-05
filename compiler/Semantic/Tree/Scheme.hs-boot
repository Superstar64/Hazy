{-# LANGUAGE RoleAnnotations #-}

module Semantic.Tree.Scheme where

import Data.Kind (Type)
import Semantic.Scope (Environment)
import Semantic.Stage (Stage)

type role Scheme representational nominal nominal

type Scheme :: Type -> Stage -> Environment -> Type
data Scheme position stage scope
