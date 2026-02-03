{-# LANGUAGE RoleAnnotations #-}

module Stage3.Check.TypeAnnotation where

import Data.Kind (Type)
import Stage2.Scope (Environment)

type role GlobalTypeAnnotation nominal

type GlobalTypeAnnotation :: Environment -> Type
data GlobalTypeAnnotation scope

type role LocalTypeAnnotation nominal nominal

type LocalTypeAnnotation :: Type -> Environment -> Type
data LocalTypeAnnotation s scope
