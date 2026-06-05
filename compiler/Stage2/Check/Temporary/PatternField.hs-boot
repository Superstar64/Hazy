{-# LANGUAGE RoleAnnotations #-}

module Stage2.Check.Temporary.PatternField where

import Data.Kind (Type)
import Stage2.Scope (Environment)

type role Field nominal nominal

type Field :: Type -> Environment -> Type
data Field s scope
