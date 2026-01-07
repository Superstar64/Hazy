{-# LANGUAGE RoleAnnotations #-}

module Stage3.Check.TypeAnnotation where

import Data.Kind (Type)
import Stage2.Scope (Environment)

type role TypeAnnotation representational nominal

type TypeAnnotation :: Type -> Environment -> Type
data TypeAnnotation inferred scope
