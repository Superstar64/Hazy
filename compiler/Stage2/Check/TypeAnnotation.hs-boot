{-# LANGUAGE RoleAnnotations #-}

module Stage2.Check.TypeAnnotation where

import Data.Kind (Type)
import Stage2.Scope (Environment)

type role TypeAnnotation nominal

type TypeAnnotation :: Environment -> Type
data TypeAnnotation scope
