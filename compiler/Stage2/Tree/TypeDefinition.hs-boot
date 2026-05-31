{-# LANGUAGE RoleAnnotations #-}

module Stage2.Tree.TypeDefinition where

import Data.Kind (Type)
import Stage2.Scope (Environment)
import Stage2.Stage (Stage)

type role TypeDefinition nominal nominal

type TypeDefinition :: Stage -> Environment -> Type
data TypeDefinition stage scope
