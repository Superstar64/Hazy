{-# LANGUAGE RoleAnnotations #-}

module Stage3.Tree.Instance where

import Data.Kind (Type)
import Stage2.Scope (Environment)

type role Instance nominal

type Instance :: Environment -> Type
data Instance scope

instance Show (Instance scope)
