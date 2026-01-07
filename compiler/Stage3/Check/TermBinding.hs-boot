{-# LANGUAGE RoleAnnotations #-}

module Stage3.Check.TermBinding where

import Data.Kind (Type)
import Stage2.Scope (Environment)

type role TermBinding nominal nominal

type TermBinding :: Type -> Environment -> Type
data TermBinding s scope
