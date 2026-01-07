{-# LANGUAGE RoleAnnotations #-}

module Stage3.Tree.Declarations where

import Data.Kind (Type)
import Stage2.Scope (Environment)

type role Declarations nominal

type Declarations :: Environment -> Type
data Declarations scope

instance Show (Declarations scope)
