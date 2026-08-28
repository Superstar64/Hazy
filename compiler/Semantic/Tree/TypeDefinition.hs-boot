{-# LANGUAGE RoleAnnotations #-}

module Semantic.Tree.TypeDefinition where

import Data.Kind (Type)
import Semantic.Scope (Environment)
import Semantic.Stage (Stage)

type role TypeDefinition nominal nominal


type TypeDefinition :: Stage -> Environment -> Type
data TypeDefinition stage scope
