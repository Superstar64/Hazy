{-# LANGUAGE RoleAnnotations #-}

module Stage2.Tree.Type where

import qualified Data.Kind as Kind (Type)
import Stage2.Scope (Environment)
import Stage2.Stage (Stage)

type Type :: Kind.Type -> Stage -> Environment -> Kind.Type

type role Type representational nominal nominal

data Type position stage scope

instance (Show position) => Show (Type position stage scope)
