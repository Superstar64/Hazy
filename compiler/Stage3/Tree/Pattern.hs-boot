{-# LANGUAGE RoleAnnotations #-}

module Stage3.Tree.Pattern where

import Data.Kind (Type)
import Stage2.Scope (Environment)

type role Pattern nominal

type Pattern :: Environment -> Type
data Pattern scope

instance Show (Pattern scope)
