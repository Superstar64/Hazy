{-# LANGUAGE RoleAnnotations #-}

module Stage3.Tree.TypePattern where

import Data.Kind (Type)
import Stage2.Scope (Environment)
import qualified Stage4.Tree.Type as Simple

type role TypePattern nominal

type TypePattern :: Environment -> Type
data TypePattern scope

instance Show (TypePattern scope)

_type :: TypePattern scope -> Simple.Type scope
