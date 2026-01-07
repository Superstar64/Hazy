{-# LANGUAGE RoleAnnotations #-}

module Stage3.Temporary.TypePattern where

import Data.Kind (Type)
import Stage2.Scope (Environment)

type role TypePattern nominal nominal

type TypePattern :: Type -> Environment -> Type
data TypePattern s scope
