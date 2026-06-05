{-# LANGUAGE RoleAnnotations #-}

module Semantic.Check.TypeAnnotation where

import Data.Kind (Type)
import Semantic.Scope (Environment)

type role TypeAnnotation nominal

type TypeAnnotation :: Environment -> Type
data TypeAnnotation scope
