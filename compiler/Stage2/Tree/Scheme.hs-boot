{-# LANGUAGE RoleAnnotations #-}

module Stage2.Tree.Scheme where

import Data.Kind (Type)
import Stage2.Scope (Environment)
import Stage2.Stage (Stage)

type role Scheme representational nominal nominal

type Scheme :: Type -> Stage -> Environment -> Type
data Scheme position stage scope
