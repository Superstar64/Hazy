{-# LANGUAGE RoleAnnotations #-}

module Semantic.Tree.Type where

import qualified Data.Kind as Kind (Type)
import Semantic.Scope (Environment)
import Semantic.Stage (Stage)

type Type :: Kind.Type -> Stage -> Environment -> Kind.Type

type role Type representational nominal nominal

data Type position stage scope

instance (Show position) => Show (Type position stage scope)
