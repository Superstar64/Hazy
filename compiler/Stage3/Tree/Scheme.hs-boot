{-# LANGUAGE RoleAnnotations #-}

module Stage3.Tree.Scheme where

import Data.Kind (Type)
import Stage2.Scope (Environment)

type role Scheme nominal

type Scheme :: Environment -> Type
data Scheme scope
