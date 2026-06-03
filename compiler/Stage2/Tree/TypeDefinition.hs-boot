{-# LANGUAGE RoleAnnotations #-}

module Stage2.Tree.TypeDefinition where

import Data.Kind (Type)
import Stage2.Scope (Environment)
import Stage2.Stage (Stage)

type role TypeDefinition nominal nominal nominal

data Equality
  = Constructive
  | Substitutive

type Constructive = 'Constructive

type Substitutive = 'Substitutive

type TypeDefinition :: Equality -> Stage -> Environment -> Type
data TypeDefinition equality stage scope
