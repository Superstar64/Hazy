{-# LANGUAGE RoleAnnotations #-}

module Stage2.Index.Type where

import Data.Kind (Type)
import Stage2.Scope (Environment)

type role Index nominal

type Index :: Environment -> Type
data Index scopes
