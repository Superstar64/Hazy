{-# LANGUAGE RoleAnnotations #-}

module Semantic.Tree.TypeDefinition where

import Data.Kind (Type)
import Semantic.Scope (Environment)
import Semantic.Stage (Stage)

type role TypeDefinition nominal nominal nominal

data Equality
  = Constructive
  | Substitutive

type Constructive = 'Constructive

type Substitutive = 'Substitutive

type TypeDefinition :: Equality -> Stage -> Environment -> Type
data TypeDefinition equality stage scope
