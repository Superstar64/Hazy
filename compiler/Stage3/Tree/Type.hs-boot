{-# LANGUAGE RoleAnnotations #-}

module Stage3.Tree.Type where

import qualified Data.Kind as Kind
import Stage2.Scope (Environment)

type role Type nominal

type Type :: Environment -> Kind.Type
data Type scope

instance Show (Type scope)
