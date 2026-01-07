{-# LANGUAGE RoleAnnotations #-}

module Stage3.Temporary.PatternField where

import Data.Kind (Type)
import Stage2.Scope (Environment)

type role Field nominal nominal

type Field :: Type -> Environment -> Type
data Field s scope
